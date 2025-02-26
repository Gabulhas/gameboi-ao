if not rawget(_G, 'Handlers') then
    Handlers = {
        add = function(name, func) end,
        list = {},
        strict = false,
        utils = {
            hasMatchingTag = function(a, b) end,
        },
        remove = function(name) end,
    }
end
if not rawget(_G, 'utils') then
    utils = {
        deepCopy = function(t) return t end,
        validate = function() return true end
    }
end

local jsonstatus, base64 = pcall(require, ".base64")

local actions            = require "astropacks-aostandard.src.actions.mod"
local schema             = require "astropacks-aostandard.src.schema.mod"
local aolibs             = require "astropacks-aostandard.src.ao.libs"
local tables             = require "astropacks-aostandard.src.extensions.tables"
local configValidator    = require "astropacks-aostandard.src.configs.validator"

local json               = aolibs.json

local Gameboy            = require "gameboy.init"

LastError                = nil
handlers                 = {}
Emulator                 = Emulator or {}


function handlers.new_emulator()
    Emulator = {
        gameboy = Gameboy.new {},
        loaded = false,
        screen_data = nil
    }
    Emulator.gameboy:initialize()
    Emulator.gameboy:reset()
end

function handlers.init_emulator(_, response)
    local _, err = handlers.new_emulator()

    if err then
        response:output({ status = 400, body = "Error: " .. err })
    else
        response:output({ status = 200, body = "Emulator initialized" })
    end
end

function handlers.get_state(_, response)
    local state = {
        loaded = Emulator and Emulator.loaded or false,
        rom_name = Emulator and Emulator.rom_name or nil,
        last_error = LastError
    }
    response:output({ status = 200, body = json.encode(state) })
end

function handlers.load_rom(payload, response)
    handlers.new_emulator()
    local rom_data = nil

    local success, err = pcall(function()
        local rom_data_encoded = payload.Message.Data
        print(type(payload.Message.Data))
        local base64vendored = require("vendor.base64")
        rom_data = base64vendored.decode(rom_data_encoded)


        assert(#rom_data % 32768 == 0, "Invalid ROM size alignment")



        Emulator.gameboy:initialize()
        Emulator.gameboy:reset()
        Emulator.gameboy.cartridge.load(rom_data, #rom_data)
        Emulator.gameboy:reset()
        print("Title: " .. Emulator.gameboy.cartridge.header.title)

        Emulator.loaded = true
        Emulator.rom_name = Emulator.gameboy.cartridge.header.title

        Emulator.gameboy:run_until_hblank()
    end)
    if success then
        response:output({
            status = 200,
            body = "ROM loaded: " .. tostring(Emulator.rom_name),
            headers = {
                ["X-ROM-Size"] = #rom_data,
            }
        })
    else
        response:output({
            status = 500,
            body = "Load failed: " .. tostring(err),
            headers = {
                ["X-Error-Type"] = "CartridgeValidation"
            }
        })
    end
end

function handlers.debug_console(payload, response)
    if not Emulator or not Emulator.loaded then
        response:output({ status = 400, body = "Load ROM first" })
        return
    end

    local command = payload.Message.Tags["X-Command"]
    local args = json.decode(payload.Message.Data or "{}")

    local result = {}
    local env = {
        gb = Emulator.gameboy,
        mem = function(addr) return Emulator.gameboy.memory[addr] end,
        reg = function()
            return {
                A = Emulator.gameboy.cpu.A,
                F = Emulator.gameboy.cpu.F,
                PC = Emulator.gameboy.cpu.PC
            }
        end
    }

    local fn, compile_err = load("return " .. command, "debug", "t", env)
    if not fn then
        fn, compile_err = load(command, "debug", "t", env)
    end

    if fn then
        result = { pcall(fn, table.unpack(args)) }
    else
        result = { false, compile_err }
    end

    response:output({
        status = 200,
        body = json.encode({
            command = command,
            success = result[1],
            output = { table.unpack(result, 2) }
        })
    })
end

function handlers.get_screen(_, response)
    if not Emulator or not Emulator.loaded then
        response:output({ status = 400, body = "No ROM loaded" })
        return
    end

    local pixels = Emulator.gameboy.graphics.game_screen
    local raw_data = {}
    for y = 0, 143 do
        for x = 0, 159 do
            local idx = (y * 160 + x) * 3 + 1
            raw_data[idx] = pixels[y][x][1]
            raw_data[idx + 1] = pixels[y][x][2]
            raw_data[idx + 2] = pixels[y][x][3]
        end
    end

    response:output({
        status = 200,
        headers = {
            ["Content-Type"] = "application/octet-stream",
            ["X-Image-Specs"] = "160x144x24" -- WxHxDepth
        },
        body = base64.encode(string.char(table.unpack(raw_data)))
    })
end

function handlers.step_clock(payload, response)
    local cycles = payload.Message.Tags["X-Cycles"] and tonumber(payload.Message.Tags["X-Cycles"]) or 1
    for _ = 1, cycles do
        Emulator.gameboy:step()
        Emulator.gameboy.graphics:update(1)
    end
    response:output({ status = 200, body = "Advanced " .. cycles .. " cycles for game " .. Emulator.rom_name })
end

function handlers.get_frame(payload, response)
    local lcd = Emulator.gameboy.graphics.lcd
    local fb_base64 = base64.encode(lcd.frame_buffer)
    response:output({
        status = 200,
        headers = {
            ["X-Frame-Width"] = 160,
            ["X-Frame-Height"] = 144,
            ["X-Frame-Format"] = "RGB24"
        },
        data = fb_base64
    })
end

function handlers.run_vblank(_, response)
    Emulator.gameboy:run_until_vblank()
    response:output({ status = 200, body = "Ran to VBlank" })
end

local input_mappings = {
    up = "Up",
    down = "Down",
    left = "Left",
    right = "Right",
    x = "A",
    z = "B",
    ["return"] = "Start",
    rshift = "Select"
}

function handlers.key_input(payload, response)
    if not Emulator.loaded then
        response:output({ status = 400, body = "Load ROM first" })
        return
    end

    local key = payload.Message.Tags["X-Key"]
    local state = payload.Message.Tags["X-State"]
    local button = input_mappings[key]

    if button and (state == "1" or state == "0") then
        Emulator.gameboy.input.keys[button] = tonumber(state)
        Emulator.gameboy.input.update()
        response:output({ status = 200 })
    else
        response:output({ status = 400, body = "Invalid key/state" })
    end
end

actions.add("InitEmulator", { function(payload, response) handlers.init_emulator(payload, response) end })
actions.add("LoadROM", { function(payload, response) handlers.load_rom(payload, response) end })
actions.add("GetState", { function(payload, response) handlers.get_state(payload, response) end })
actions.add("DebugConsole", { function(payload, response) handlers.debug_console(payload, response) end })
actions.add("GetScreen", { function(payload, response) handlers.get_screen(payload, response) end })
actions.add("StepClock", { function(payload, response) handlers.step_clock(payload, response) end })
actions.add("GetFrame", { function(payload, response) handlers.get_frame(payload, response) end })
actions.add("RunVBlank", { function(payload, response) handlers.run_vblank(payload, response) end })
actions.add("KeyInput", { function(payload, response) handlers.key_input(payload, response) end })


pcall(require, "vendor.base64")
pcall(require, "vendor.bitop-lua")
