do
local _ENV = _ENV
package.preload[ ".base64" ] = function( ... ) local arg = _G.arg;
--[[

 base64 -- v1.5.3 public domain Lua base64 encoder/decoder
 no warranty implied; use at your own risk

 Needs bit32.extract function. If not present it's implemented using BitOp
 or Lua 5.3 native bit operators. For Lua 5.1 fallbacks to pure Lua
 implementation inspired by Rici Lake's post:
   http://ricilake.blogspot.co.uk/2007/10/iterating-bits-in-lua.html

 author: Ilya Kolbin (iskolbin@gmail.com)
 url: github.com/iskolbin/lbase64

 COMPATIBILITY

 Lua 5.1+, LuaJIT

 LICENSE

 See end of file for license information.

--]]


local base64 = {}

_G.bit = require("vendor.bitop-lua").bit32

local extract = require("vendor.bitop-lua").extract -- Lua 5.2/Lua 5.3 in compatibility mode
if not extract then
    if _G.bit then                                  -- LuaJIT
        local shl, shr, band = _G.bit.lshift, _G.bit.rshift, _G.bit.band
        extract = function(v, from, width)
            return band(shr(v, from), shl(1, width) - 1)
        end
    elseif _G._VERSION == "Lua 5.1" then
        extract = function(v, from, width)
            local w = 0
            local flag = 2 ^ from
            for i = 0, width - 1 do
                local flag2 = flag + flag
                if v % flag2 >= flag then
                    w = w + 2 ^ i
                end
                flag = flag2
            end
            return w
        end
    else -- Lua 5.3+
        extract = load [[return function( v, from, width )
			return ( v >> from ) & ((1 << width) - 1)
		end]]()
    end
end


function base64.makeencoder(s62, s63, spad)
    local encoder = {}
    for b64code, char in pairs { [0] = 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
        'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y',
        'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
        'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2',
        '3', '4', '5', '6', '7', '8', '9', s62 or '+', s63 or '/', spad or '=' } do
        encoder[b64code] = char:byte()
    end
    return encoder
end

function base64.makedecoder(s62, s63, spad)
    local decoder = {}
    for b64code, charcode in pairs(base64.makeencoder(s62, s63, spad)) do
        decoder[charcode] = b64code
    end
    return decoder
end

local DEFAULT_ENCODER = base64.makeencoder()
local DEFAULT_DECODER = base64.makedecoder()

local char, concat = string.char, table.concat

function base64.encode(str, encoder, usecaching)
    encoder = encoder or DEFAULT_ENCODER
    local t, k, n = {}, 1, #str
    local lastn = n % 3
    local cache = {}
    for i = 1, n - lastn, 3 do
        local a, b, c = str:byte(i, i + 2)
        local v = a * 0x10000 + b * 0x100 + c
        local s
        if usecaching then
            s = cache[v]
            if not s then
                s = char(encoder[extract(v, 18, 6)], encoder[extract(v, 12, 6)], encoder[extract(v, 6, 6)],
                    encoder[extract(v, 0, 6)])
                cache[v] = s
            end
        else
            s = char(encoder[extract(v, 18, 6)], encoder[extract(v, 12, 6)], encoder[extract(v, 6, 6)],
                encoder[extract(v, 0, 6)])
        end
        t[k] = s
        k = k + 1
    end
    if lastn == 2 then
        local a, b = str:byte(n - 1, n)
        local v = a * 0x10000 + b * 0x100
        t[k] = char(encoder[extract(v, 18, 6)], encoder[extract(v, 12, 6)], encoder[extract(v, 6, 6)], encoder[64])
    elseif lastn == 1 then
        local v = str:byte(n) * 0x10000
        t[k] = char(encoder[extract(v, 18, 6)], encoder[extract(v, 12, 6)], encoder[64], encoder[64])
    end
    return concat(t)
end

function base64.decode(b64, decoder, usecaching)
    decoder = decoder or DEFAULT_DECODER
    local pattern = '[^%w%+%/%=]'
    if decoder then
        local s62, s63
        for charcode, b64code in pairs(decoder) do
            if b64code == 62 then
                s62 = charcode
            elseif b64code == 63 then
                s63 = charcode
            end
        end
        pattern = ('[^%%w%%%s%%%s%%=]'):format(char(s62), char(s63))
    end
    b64 = b64:gsub(pattern, '')
    local cache = usecaching and {}
    local t, k = {}, 1
    local n = #b64
    local padding = b64:sub(-2) == '==' and 2 or b64:sub(-1) == '=' and 1 or 0
    for i = 1, padding > 0 and n - 4 or n, 4 do
        local a, b, c, d = b64:byte(i, i + 3)
        local s
        if usecaching then
            local v0 = a * 0x1000000 + b * 0x10000 + c * 0x100 + d
            s = cache[v0]
            if not s then
                local v = decoder[a] * 0x40000 + decoder[b] * 0x1000 + decoder[c] * 0x40 + decoder[d]
                s = char(extract(v, 16, 8), extract(v, 8, 8), extract(v, 0, 8))
                cache[v0] = s
            end
        else
            local v = decoder[a] * 0x40000 + decoder[b] * 0x1000 + decoder[c] * 0x40 + decoder[d]
            s = char(extract(v, 16, 8), extract(v, 8, 8), extract(v, 0, 8))
        end
        t[k] = s
        k = k + 1
    end
    if padding == 1 then
        local a, b, c = b64:byte(n - 3, n - 1)
        local v = decoder[a] * 0x40000 + decoder[b] * 0x1000 + decoder[c] * 0x40
        t[k] = char(extract(v, 16, 8), extract(v, 8, 8))
    elseif padding == 2 then
        local a, b = b64:byte(n - 3, n - 2)
        local v = decoder[a] * 0x40000 + decoder[b] * 0x1000
        t[k] = char(extract(v, 16, 8))
    end
    return concat(t)
end

return base64

--[[
------------------------------------------------------------------------------
This software is available under 2 licenses -- choose whichever you prefer.
------------------------------------------------------------------------------
ALTERNATIVE A - MIT License
Copyright (c) 2018 Ilya Kolbin
Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
------------------------------------------------------------------------------
ALTERNATIVE B - Public Domain (www.unlicense.org)
This is free and unencumbered software released into the public domain.
Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
software, either in source code form or as a compiled binary, for any purpose,
commercial or non-commercial, and by any means.
In jurisdictions that recognize copyright laws, the author or authors of this
software dedicate any and all copyright interest in the software to the public
domain. We make this dedication for the benefit of the public at large and to
the detriment of our heirs and successors. We intend this dedication to be an
overt act of relinquishment in perpetuity of all present and future rights to
this software under copyright law.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
------------------------------------------------------------------------------
--]]
end
end

do
local _ENV = _ENV
package.preload[ "arweave.types.type" ] = function( ... ) local arg = _G.arg;
---@class Type
local Type = {
  -- custom name for the defined type
  ---@type string|nil
  name = nil,
  -- list of assertions to perform on any given value
  ---@type { message: string, validate: fun(val: any): boolean }[]
  conditions = nil
}

-- Execute an assertion for a given value
---@param val any Value to assert for
---@param message string? Optional message to throw
---@param no_error boolean? Optionally disable error throwing (will return boolean)
function Type:assert(val, message, no_error)
  for _, condition in ipairs(self.conditions) do
    if not condition.validate(val) then
      if no_error then
        return false
      end
      self:error(message or condition.message)
    end
  end

  if no_error then
    return true
  end
end

-- Add a custom condition/assertion to assert for
---@param message string Error message for the assertion
---@param assertion fun(val: any): boolean Custom assertion function that is asserted with the provided value
function Type:custom(message, assertion)
  -- condition to add
  local condition = {
    message = message,
    validate = assertion
  }

  -- new instance if there are no conditions yet
  if self.conditions == nil then
    local instance = {
      conditions = {}
    }

    table.insert(instance.conditions, condition)
    setmetatable(instance, self)
    self.__index = self

    return instance
  end

  table.insert(self.conditions, condition)
  return self
end

-- Add an assertion for built in types
---@param t "nil"|"number"|"string"|"boolean"|"table"|"function"|"thread"|"userdata" Type to assert for
---@param message string? Optional assertion error message
function Type:type(t, message)
  return self:custom(message or ("Not of type (" .. t .. ")"), function(val)
    return type(val) == t
  end)
end

-- Type must be userdata
---@param message string? Optional assertion error message
function Type:userdata(message)
  return self:type("userdata", message)
end

-- Type must be thread
---@param message string? Optional assertion error message
function Type:thread(message)
  return self:type("thread", message)
end

-- Type must be table
---@param message string? Optional assertion error message
function Type:table(message)
  return self:type("table", message)
end

-- Table's keys must be of type t
---@param t Type Type to assert the keys for
---@param message string? Optional assertion error message
function Type:keys(t, message)
  return self:custom(message or "Invalid table keys", function(val)
    if type(val) ~= "table" then
      return false
    end

    for key, _ in pairs(val) do
      -- check if the assertion throws any errors
      local success = pcall(function()
        return t:assert(key)
      end)

      if not success then
        return false
      end
    end

    return true
  end)
end

-- Type must be array
---@param message string? Optional assertion error message
function Type:array(message)
  return self:table():keys(Type:number(), message)
end

-- Table's values must be of type t
---@param t Type Type to assert the values for
---@param message string? Optional assertion error message
function Type:values(t, message)
  return self:custom(message or "Invalid table values", function(val)
    if type(val) ~= "table" then
      return false
    end

    for _, v in pairs(val) do
      -- check if the assertion throws any errors
      local success = pcall(function()
        return t:assert(v)
      end)

      if not success then
        return false
      end
    end

    return true
  end)
end

-- Type must be boolean
---@param message string? Optional assertion error message
function Type:boolean(message)
  return self:type("boolean", message)
end

-- Type must be function
---@param message string? Optional assertion error message
function Type:_function(message)
  return self:type("function", message)
end

-- Type must be nil
---@param message string? Optional assertion error message
function Type:_nil(message)
  return self:type("nil", message)
end

-- Value must be the same
---@param val any The value the assertion must be made with
---@param message string? Optional assertion error message
function Type:is(val, message)
  return self:custom(message
                       or "Value did not match expected value (Type:is(expected))",
                     function(v)
    return v == val
  end)
end

-- Type must be string
---@param message string? Optional assertion error message
function Type:string(message)
  return self:type("string", message)
end

-- String type must match pattern
---@param pattern string Pattern to match
---@param message string? Optional assertion error message
function Type:match(pattern, message)
  return self:custom(message
                       or ("String did not match pattern \"" .. pattern .. "\""),
                     function(val)
    return string.match(val, pattern) ~= nil
  end)
end

-- String type must be of defined length
---@param len number Required length
---@param match_type? "less"|"greater" String length should be "less" than or "greater" than the defined length. Leave empty for exact match.
---@param message string? Optional assertion error message
function Type:length(len, match_type, message)
  local match_msgs = {
    less = "String length is not less than " .. len,
    greater = "String length is not greater than " .. len,
    default = "String is not of length " .. len
  }

  return self:custom(message or (match_msgs[match_type] or match_msgs.default),
                     function(val)
    local strlen = string.len(val)

    -- validate length
    if match_type == "less" then
      return strlen < len
    elseif match_type == "greater" then
      return strlen > len
    end

    return strlen == len
  end)
end

-- Type must be a number
---@param message string? Optional assertion error message
function Type:number(message)
  return self:type("number", message)
end

-- Number must be an integer (chain after "number()")
---@param message string? Optional assertion error message
function Type:integer(message)
  return self:custom(message or "Number is not an integer", function(val)
    return val % 1 == 0
  end)
end

-- Number must be even (chain after "number()")
---@param message string? Optional assertion error message
function Type:even(message)
  return self:custom(message or "Number is not even", function(val)
    return val % 2 == 0
  end)
end

-- Number must be odd (chain after "number()")
---@param message string? Optional assertion error message
function Type:odd(message)
  return self:custom(message or "Number is not odd", function(val)
    return val % 2 == 1
  end)
end

-- Number must be less than the number "n" (chain after "number()")
---@param n number Number to compare with
---@param message string? Optional assertion error message
function Type:less_than(n, message)
  return self:custom(message or ("Number is not less than " .. n), function(val)
    return val < n
  end)
end

-- Number must be greater than the number "n" (chain after "number()")
---@param n number Number to compare with
---@param message string? Optional assertion error message
function Type:greater_than(n, message)
  return self:custom(message or ("Number is not greater than" .. n),
                     function(val)
    return val > n
  end)
end

-- Make a type optional (allow them to be nil apart from the required type)
---@param t Type Type to assert for if the value is not nil
---@param message string? Optional assertion error message
function Type:optional(t, message)
  return self:custom(message or "Optional type did not match", function(val)
    if val == nil then
      return true
    end

    t:assert(val)
    return true
  end)
end

-- Table must be of object
---@param obj { [any]: Type }
---@param strict? boolean Only allow the defined keys from the object, throw error on other keys (false by default)
---@param message string? Optional assertion error message
function Type:object(obj, strict, message)
  if type(obj) ~= "table" then
    self:error(
      "Invalid object structure provided for object assertion (has to be a table):\n"
        .. tostring(obj))
  end

  return self:custom(message
                       or ("Not of defined object (" .. tostring(obj) .. ")"),
                     function(val)
    if type(val) ~= "table" then
      return false
    end

    -- for each value, validate
    for key, assertion in pairs(obj) do
      if val[key] == nil then
        return false
      end

      -- check if the assertion throws any errors
      local success = pcall(function()
        return assertion:assert(val[key])
      end)

      if not success then
        return false
      end
    end

    -- in strict mode, we do not allow any other keys
    if strict then
      for key, _ in pairs(val) do
        if obj[key] == nil then
          return false
        end
      end
    end

    return true
  end)
end

-- Type has to be either one of the defined assertions
---@param ... Type Type(s) to assert for
function Type:either(...)
  ---@type Type[]
  local assertions = {
    ...
  }

  return self:custom("Neither types matched defined in (Type:either(...))",
                     function(val)
    for _, assertion in ipairs(assertions) do
      if pcall(function()
        return assertion:assert(val)
      end) then
        return true
      end
    end

    return false
  end)
end

-- Type cannot be the defined assertion (tip: for multiple negated assertions, use Type:either(...))
---@param t Type Type to NOT assert for
---@param message string? Optional assertion error message
function Type:is_not(t, message)
  return self:custom(message
                       or "Value incorrectly matched with the assertion provided (Type:is_not())",
                     function(val)
    local success = pcall(function()
      return t:assert(val)
    end)

    return not success
  end)
end

-- Set the name of the custom type
-- This will be used with error logs
---@param name string Name of the type definition
function Type:set_name(name)
  self.name = name
  return self
end

-- Throw an error
---@param message any Message to log
---@private
function Type:error(message)
  error("[Type " .. (self.name or tostring(self.__index)) .. "] "
          .. tostring(message))
end

return Type
end
end

do
local _ENV = _ENV
package.preload[ "astropacks-aostandard.src.actions.mod" ] = function( ... ) local arg = _G.arg;
local runtime = require "astropacks-aostandard.src.runtime.mod"
local Response = require "astropacks-aostandard.src.actions.response"
local ext = require "astropacks-aostandard.src.extensions.mod"

local mod = {}

---Clean up the given error assuming it was thrown using `runtime.throw()`.
---
---@param err? any
local function clean_error(err)
  if type(err) == "string" then
    return err
  end

  if type(err) == "table" then
    if err.message ~= nil then
      return string.gsub(err.message, ".+message_body|", "")
    end
  end

  return err
end

---Convert handler messages to a uniform payload object expected by the handler
---functions.
---
---@param msg Message The message argument sent by ao to convert to a payload
---object.
---@param env Message The environment argument sent by ao.
---@return Payload
local function msg_to_payload(msg, env)
  ---@type table<string, any>
  return {
    -- Define 'From' as 'Caller' for semantics
    Caller = msg.From,

    -- Keep message and env variables intact
    Message = msg,
    Environment = env,

    -- Add Action for convenience
    Action = msg.Tags.Action,
  }
end

---Wrapper to shorten `hasMatchingTag` condition call.
---
---@param action string
---@return boolean|fun(message: Message):boolean|"break"|"continue"|"skip"
local function action_is(action)
  return Handlers.utils.hasMatchingTag("Action", action)
end

---Add an action to this process.
---
---If an error occurs, this action will throw an `Error` object.
---
---@param action string The name of the action that gets passed via `Action = "<action>"`.
---@param ... ActionHandler[]
function mod.add(action, ...)
  if not Handlers then
    error("Cannot create handler action. _G['Handlers'] does not exist")
  end

  local handlers = ...

  Handlers.add(action, action_is(action), function(msg, env)

    -- Normalize the payload
    local payload = msg_to_payload(msg or {}, env or {})

    -- Standardize the way `Results` are created
    local response = Response.new()
    
    response:schema({
      process_id = ao.id,
      action = action,
      caller = payload.Caller,
      message_id = payload.Message.Id,
    })

    -- Call the handler function with the normalized payload
    -- It is wrapped in this pcall so the standardized response (above) can be
    -- used in outputs
    local status, err = pcall(function()
      if type(handlers) ~= "table" or #handlers == 0 then
        runtime.throw("Action '" .. action .. "' does not have handlers")
        return
      end

      for i = 1, #handlers, 1 do
        local handlerFn = handlers[i]

        if handlerFn == nil then
          runtime.throw("Action '" .. action .. "' handler at position #" .. i .. " returned nil")
          return
        end

        handlerFn(payload, response)
      end
    end)

    local cleanErrorMessage = clean_error(err)

    if err and not status then
      response:error(cleanErrorMessage)
    end
  end)
end

return mod
end
end

do
local _ENV = _ENV
package.preload[ "astropacks-aostandard.src.actions.response" ] = function( ... ) local arg = _G.arg;
local aolibs = require "astropacks-aostandard.src.ao.libs"
local output = require "astropacks-aostandard.src.output.mod"

local json = aolibs.json

local mod = {}

local function build_json_response(schema, data)
    local clone = {}

    if type(schema) ~= "table" then
        schema = {}
    end

    for key, value in pairs(schema) do
        clone[key] = value
    end

    if #clone == 0 then
        clone.data = data
    else
        table.insert(clone, #clone, data)
    end

    return clone
end

---@class Response
local Response = {
    SCHEMA = {}
}

---Set this response's schema.
---
---@param schema? any
---@return self
function Response:schema(schema)
    self.SCHEMA = schema or {}
    return self
end

---Print the given `data` with this instance's fields as JSON.
---
---@param data? any
---@return self
function Response:output(data)
    local jsonResponse = build_json_response(self.SCHEMA, data)

    --- Output it as JSON
    output.json(jsonResponse)

    return self
end

---@param data? any
---@return self
function Response:error(data)
    local jsonResponse = build_json_response(self.SCHEMA, data)

    jsonResponse.error = data
    jsonResponse.data = nil

    --- Output it as JSON
    output.json(jsonResponse)

    return self
end

---Create a JSON string.
---@param data? any
---@return string
function Response:create_json(data)
    return json.encode(build_json_response(self.SCHEMA, data))
end

---@generic Fields
---@return Response
function mod.new()
    local self = {}

    setmetatable(
        self,
        { __index = Response }
    )

    return self
end

return mod
end
end

do
local _ENV = _ENV
package.preload[ "astropacks-aostandard.src.ao.bint" ] = function( ... ) local arg = _G.arg;
--[[--
lua-bint - v0.5.1 - 26/Jun/2023
Eduardo Bart - edub4rt@gmail.com
https://github.com/edubart/lua-bint

Small portable arbitrary-precision integer arithmetic library in pure Lua for
computing with large integers.

Different from most arbitrary-precision integer libraries in pure Lua out there this one
uses an array of lua integers as underlying data-type in its implementation instead of
using strings or large tables, this make it efficient for working with fixed width integers
and to make bitwise operations.

## Design goals

The main design goal of this library is to be small, correct, self contained and use few
resources while retaining acceptable performance and feature completeness.

The library is designed to follow recent Lua integer semantics, this means that
integer overflow warps around,
signed integers are implemented using two-complement arithmetic rules,
integer division operations rounds towards minus infinity,
any mixed operations with float numbers promotes the value to a float,
and the usual division/power operation always promotes to floats.

The library is designed to be possible to work with only unsigned integer arithmetic
when using the proper methods.

All the lua arithmetic operators (+, -, *, //, /, %) and bitwise operators (&, |, ~, <<, >>)
are implemented as metamethods.

The integer size must be fixed in advance and the library is designed to be more efficient when
working with integers of sizes between 64-4096 bits. If you need to work with really huge numbers
without size restrictions then use another library. This choice has been made to have more efficiency
in that specific size range.

## Usage

First on you should require the bint file including how many bits the bint module will work with,
by calling the returned function from the require, for example:

```lua
local bint = require 'bint'(1024)
```

For more information about its arguments see @{newmodule}.
Then when you need create a bint, you can use one of the following functions:

* @{bint.fromuinteger} (convert from lua integers, but read as unsigned integer)
* @{bint.frominteger} (convert from lua integers, preserving the sign)
* @{bint.frombase} (convert from arbitrary bases, like hexadecimal)
* @{bint.fromstring} (convert from arbitrary string, support binary/hexadecimal/decimal)
* @{bint.trunc} (convert from lua numbers, truncating the fractional part)
* @{bint.new} (convert from anything, asserts on invalid integers)
* @{bint.tobint} (convert from anything, returns nil on invalid integers)
* @{bint.parse} (convert from anything, returns a lua number as fallback)
* @{bint.zero}
* @{bint.one}
* `bint`

You can also call `bint` as it is an alias to `bint.new`.
In doubt use @{bint.new} to create a new bint.

Then you can use all the usual lua numeric operations on it,
all the arithmetic metamethods are implemented.
When you are done computing and need to get the result,
get the output from one of the following functions:

* @{bint.touinteger} (convert to a lua integer, wraps around as an unsigned integer)
* @{bint.tointeger} (convert to a lua integer, wraps around, preserves the sign)
* @{bint.tonumber} (convert to lua float, losing precision)
* @{bint.tobase} (convert to a string in any base)
* @{bint.__tostring} (convert to a string in base 10)

To output a very large integer with no loss you probably want to use @{bint.tobase}
or call `tostring` to get a string representation.

## Precautions

All library functions can be mixed with lua numbers,
this makes easy to mix operations between bints and lua numbers,
however the user should take care in some situations:

* Don't mix integers and float operations if you want to work with integers only.
* Don't use the regular equal operator ('==') to compare values from this library,
unless you know in advance that both values are of the same primitive type,
otherwise it will always return false, use @{bint.eq} to be safe.
* Don't pass fractional numbers to functions that an integer is expected
* Don't mix operations between bint classes with different sizes as this is not supported, this
will throw assertions.
* Remember that casting back to lua integers or numbers precision can be lost.
* For dividing while preserving integers use the @{bint.__idiv} (the '//' operator).
* For doing power operation preserving integers use the @{bint.ipow} function.
* Configure the proper integer size you intend to work with, otherwise large integers may wrap around.

]]

-- Returns number of bits of the internal lua integer type.
local function luainteger_bitsize()
  local n, i = -1, 0
  repeat
    n, i = n >> 16, i + 16
  until n==0
  return i
end

local math_type = math.type
local math_floor = math.floor
local math_abs = math.abs
local math_ceil = math.ceil
local math_modf = math.modf
local math_mininteger = math.mininteger
local math_maxinteger = math.maxinteger
local math_max = math.max
local math_min = math.min
local string_format = string.format
local table_insert = table.insert
local table_concat = table.concat
local table_unpack = table.unpack

local memo = {}

--- Create a new bint module representing integers of the desired bit size.
-- This is the returned function when `require 'bint'` is called.
-- @function newmodule
-- @param bits Number of bits for the integer representation, must be multiple of wordbits and
-- at least 64.
-- @param[opt] wordbits Number of the bits for the internal word,
-- defaults to half of Lua's integer size.
local function newmodule(bits, wordbits)

local intbits = luainteger_bitsize()
bits = bits or 256
wordbits = wordbits or (intbits // 2)

-- Memoize bint modules
local memoindex = bits * 64 + wordbits
if memo[memoindex] then
  return memo[memoindex]
end

-- Validate
assert(bits % wordbits == 0, 'bitsize is not multiple of word bitsize')
assert(2*wordbits <= intbits, 'word bitsize must be half of the lua integer bitsize')
assert(bits >= 64, 'bitsize must be >= 64')
assert(wordbits >= 8, 'wordbits must be at least 8')
assert(bits % 8 == 0, 'bitsize must be multiple of 8')

-- Create bint module
local bint = {}
bint.__index = bint

--- Number of bits representing a bint instance.
bint.bits = bits

-- Constants used internally
local BINT_BITS = bits
local BINT_BYTES = bits // 8
local BINT_WORDBITS = wordbits
local BINT_SIZE = BINT_BITS // BINT_WORDBITS
local BINT_WORDMAX = (1 << BINT_WORDBITS) - 1
local BINT_WORDMSB = (1 << (BINT_WORDBITS - 1))
local BINT_LEPACKFMT = '<'..('I'..(wordbits // 8)):rep(BINT_SIZE)
local BINT_MATHMININTEGER, BINT_MATHMAXINTEGER
local BINT_MININTEGER

--- Create a new bint with 0 value.
function bint.zero()
  local x = setmetatable({}, bint)
  for i=1,BINT_SIZE do
    x[i] = 0
  end
  return x
end
local bint_zero = bint.zero

--- Create a new bint with 1 value.
function bint.one()
  local x = setmetatable({}, bint)
  x[1] = 1
  for i=2,BINT_SIZE do
    x[i] = 0
  end
  return x
end
local bint_one = bint.one

-- Convert a value to a lua integer without losing precision.
local function tointeger(x)
  x = tonumber(x)
  local ty = math_type(x)
  if ty == 'float' then
    local floorx = math_floor(x)
    if floorx == x then
      x = floorx
      ty = math_type(x)
    end
  end
  if ty == 'integer' then
    return x
  end
end

--- Create a bint from an unsigned integer.
-- Treats signed integers as an unsigned integer.
-- @param x A value to initialize from convertible to a lua integer.
-- @return A new bint or nil in case the input cannot be represented by an integer.
-- @see bint.frominteger
function bint.fromuinteger(x)
  x = tointeger(x)
  if x then
    if x == 1 then
      return bint_one()
    elseif x == 0 then
      return bint_zero()
    end
    local n = setmetatable({}, bint)
    for i=1,BINT_SIZE do
      n[i] = x & BINT_WORDMAX
      x = x >> BINT_WORDBITS
    end
    return n
  end
end
local bint_fromuinteger = bint.fromuinteger

--- Create a bint from a signed integer.
-- @param x A value to initialize from convertible to a lua integer.
-- @return A new bint or nil in case the input cannot be represented by an integer.
-- @see bint.fromuinteger
function bint.frominteger(x)
  x = tointeger(x)
  if x then
    if x == 1 then
      return bint_one()
    elseif x == 0 then
      return bint_zero()
    end
    local neg = false
    if x < 0 then
      x = math_abs(x)
      neg = true
    end
    local n = setmetatable({}, bint)
    for i=1,BINT_SIZE do
      n[i] = x & BINT_WORDMAX
      x = x >> BINT_WORDBITS
    end
    if neg then
      n:_unm()
    end
    return n
  end
end
local bint_frominteger = bint.frominteger

local basesteps = {}

-- Compute the read step for frombase function
local function getbasestep(base)
  local step = basesteps[base]
  if step then
    return step
  end
  step = 0
  local dmax = 1
  local limit = math_maxinteger // base
  repeat
    step = step + 1
    dmax = dmax * base
  until dmax >= limit
  basesteps[base] = step
  return step
end

-- Compute power with lua integers.
local function ipow(y, x, n)
  if n == 1 then
    return y * x
  elseif n & 1 == 0 then --even
    return ipow(y, x * x, n // 2)
  end
  return ipow(x * y, x * x, (n-1) // 2)
end

--- Create a bint from a string of the desired base.
-- @param s The string to be converted from,
-- must have only alphanumeric and '+-' characters.
-- @param[opt] base Base that the number is represented, defaults to 10.
-- Must be at least 2 and at most 36.
-- @return A new bint or nil in case the conversion failed.
function bint.frombase(s, base)
  if type(s) ~= 'string' then
    return
  end
  base = base or 10
  if not (base >= 2 and base <= 36) then
    -- number base is too large
    return
  end
  local step = getbasestep(base)
  if #s < step then
    -- string is small, use tonumber (faster)
    return bint_frominteger(tonumber(s, base))
  end
  local sign, int = s:lower():match('^([+-]?)(%w+)$')
  if not (sign and int) then
    -- invalid integer string representation
    return
  end
  local n = bint_zero()
  for i=1,#int,step do
    local part = int:sub(i,i+step-1)
    local d = tonumber(part, base)
    if not d then
      -- invalid integer string representation
      return
    end
    if i > 1 then
      n = n * ipow(1, base, #part)
    end
    if d ~= 0 then
      n:_add(d)
    end
  end
  if sign == '-' then
    n:_unm()
  end
  return n
end
local bint_frombase = bint.frombase

--- Create a new bint from a string.
-- The string can by a decimal number, binary number prefixed with '0b' or hexadecimal number prefixed with '0x'.
-- @param s A string convertible to a bint.
-- @return A new bint or nil in case the conversion failed.
-- @see bint.frombase
function bint.fromstring(s)
  if type(s) ~= 'string' then
    return
  end
  if s:find('^[+-]?[0-9]+$') then
    return bint_frombase(s, 10)
  elseif s:find('^[+-]?0[xX][0-9a-fA-F]+$') then
    return bint_frombase(s:gsub('0[xX]', '', 1), 16)
  elseif s:find('^[+-]?0[bB][01]+$') then
    return bint_frombase(s:gsub('0[bB]', '', 1), 2)
  end
end
local bint_fromstring = bint.fromstring

--- Create a new bint from a buffer of little-endian bytes.
-- @param buffer Buffer of bytes, extra bytes are trimmed from the right, missing bytes are padded to the right.
-- @raise An assert is thrown in case buffer is not an string.
-- @return A bint.
function bint.fromle(buffer)
  assert(type(buffer) == 'string', 'buffer is not a string')
  if #buffer > BINT_BYTES then -- trim extra bytes from the right
    buffer = buffer:sub(1, BINT_BYTES)
  elseif #buffer < BINT_BYTES then -- add missing bytes to the right
    buffer = buffer..('\x00'):rep(BINT_BYTES - #buffer)
  end
  return setmetatable({BINT_LEPACKFMT:unpack(buffer)}, bint)
end

--- Create a new bint from a buffer of big-endian bytes.
-- @param buffer Buffer of bytes, extra bytes are trimmed from the left, missing bytes are padded to the left.
-- @raise An assert is thrown in case buffer is not an string.
-- @return A bint.
function bint.frombe(buffer)
  assert(type(buffer) == 'string', 'buffer is not a string')
  if #buffer > BINT_BYTES then -- trim extra bytes from the left
    buffer = buffer:sub(-BINT_BYTES, #buffer)
  elseif #buffer < BINT_BYTES then -- add missing bytes to the left
    buffer = ('\x00'):rep(BINT_BYTES - #buffer)..buffer
  end
  return setmetatable({BINT_LEPACKFMT:unpack(buffer:reverse())}, bint)
end

--- Create a new bint from a value.
-- @param x A value convertible to a bint (string, number or another bint).
-- @return A new bint, guaranteed to be a new reference in case needed.
-- @raise An assert is thrown in case x is not convertible to a bint.
-- @see bint.tobint
-- @see bint.parse
function bint.new(x)
  if getmetatable(x) ~= bint then
    local ty = type(x)
    if ty == 'number' then
      x = bint_frominteger(x)
    elseif ty == 'string' then
      x = bint_fromstring(x)
    end
    assert(x, 'value cannot be represented by a bint')
    return x
  end
  -- return a clone
  local n = setmetatable({}, bint)
  for i=1,BINT_SIZE do
    n[i] = x[i]
  end
  return n
end
local bint_new = bint.new

--- Convert a value to a bint if possible.
-- @param x A value to be converted (string, number or another bint).
-- @param[opt] clone A boolean that tells if a new bint reference should be returned.
-- Defaults to false.
-- @return A bint or nil in case the conversion failed.
-- @see bint.new
-- @see bint.parse
function bint.tobint(x, clone)
  if getmetatable(x) == bint then
    if not clone then
      return x
    end
    -- return a clone
    local n = setmetatable({}, bint)
    for i=1,BINT_SIZE do
      n[i] = x[i]
    end
    return n
  end
  local ty = type(x)
  if ty == 'number' then
    return bint_frominteger(x)
  elseif ty == 'string' then
    return bint_fromstring(x)
  end
end
local tobint = bint.tobint

--- Convert a value to a bint if possible otherwise to a lua number.
-- Useful to prepare values that you are unsure if it's going to be an integer or float.
-- @param x A value to be converted (string, number or another bint).
-- @param[opt] clone A boolean that tells if a new bint reference should be returned.
-- Defaults to false.
-- @return A bint or a lua number or nil in case the conversion failed.
-- @see bint.new
-- @see bint.tobint
function bint.parse(x, clone)
  local i = tobint(x, clone)
  if i then
    return i
  end
  return tonumber(x)
end
local bint_parse = bint.parse

--- Convert a bint to an unsigned integer.
-- Note that large unsigned integers may be represented as negatives in lua integers.
-- Note that lua cannot represent values larger than 64 bits,
-- in that case integer values wrap around.
-- @param x A bint or a number to be converted into an unsigned integer.
-- @return An integer or nil in case the input cannot be represented by an integer.
-- @see bint.tointeger
function bint.touinteger(x)
  if getmetatable(x) == bint then
    local n = 0
    for i=1,BINT_SIZE do
      n = n | (x[i] << (BINT_WORDBITS * (i - 1)))
    end
    return n
  end
  return tointeger(x)
end

--- Convert a bint to a signed integer.
-- It works by taking absolute values then applying the sign bit in case needed.
-- Note that lua cannot represent values larger than 64 bits,
-- in that case integer values wrap around.
-- @param x A bint or value to be converted into an unsigned integer.
-- @return An integer or nil in case the input cannot be represented by an integer.
-- @see bint.touinteger
function bint.tointeger(x)
  if getmetatable(x) == bint then
    local n = 0
    local neg = x:isneg()
    if neg then
      x = -x
    end
    for i=1,BINT_SIZE do
      n = n | (x[i] << (BINT_WORDBITS * (i - 1)))
    end
    if neg then
      n = -n
    end
    return n
  end
  return tointeger(x)
end
local bint_tointeger = bint.tointeger

local function bint_assert_tointeger(x)
  x = bint_tointeger(x)
  if not x then
    error('value has no integer representation')
  end
  return x
end

--- Convert a bint to a lua float in case integer would wrap around or lua integer otherwise.
-- Different from @{bint.tointeger} the operation does not wrap around integers,
-- but digits precision are lost in the process of converting to a float.
-- @param x A bint or value to be converted into a lua number.
-- @return A lua number or nil in case the input cannot be represented by a number.
-- @see bint.tointeger
function bint.tonumber(x)
  if getmetatable(x) == bint then
    if x <= BINT_MATHMAXINTEGER and x >= BINT_MATHMININTEGER then
      return x:tointeger()
    end
    return tonumber(tostring(x))
  end
  return tonumber(x)
end
local bint_tonumber = bint.tonumber

-- Compute base letters to use in bint.tobase
local BASE_LETTERS = {}
do
  for i=1,36 do
    BASE_LETTERS[i-1] = ('0123456789abcdefghijklmnopqrstuvwxyz'):sub(i,i)
  end
end

--- Convert a bint to a string in the desired base.
-- @param x The bint to be converted from.
-- @param[opt] base Base to be represented, defaults to 10.
-- Must be at least 2 and at most 36.
-- @param[opt] unsigned Whether to output as an unsigned integer.
-- Defaults to false for base 10 and true for others.
-- When unsigned is false the symbol '-' is prepended in negative values.
-- @return A string representing the input.
-- @raise An assert is thrown in case the base is invalid.
function bint.tobase(x, base, unsigned)
  x = tobint(x)
  if not x then
    -- x is a fractional float or something else
    return
  end
  base = base or 10
  if not (base >= 2 and base <= 36) then
    -- number base is too large
    return
  end
  if unsigned == nil then
    unsigned = base ~= 10
  end
  local isxneg = x:isneg()
  if (base == 10 and not unsigned) or (base == 16 and unsigned and not isxneg) then
    if x <= BINT_MATHMAXINTEGER and x >= BINT_MATHMININTEGER then
      -- integer is small, use tostring or string.format (faster)
      local n = x:tointeger()
      if base == 10 then
        return tostring(n)
      elseif unsigned then
        return string_format('%x', n)
      end
    end
  end
  local ss = {}
  local neg = not unsigned and isxneg
  x = neg and x:abs() or bint_new(x)
  local xiszero = x:iszero()
  if xiszero then
    return '0'
  end
  -- calculate basepow
  local step = 0
  local basepow = 1
  local limit = (BINT_WORDMSB - 1) // base
  repeat
    step = step + 1
    basepow = basepow * base
  until basepow >= limit
  -- serialize base digits
  local size = BINT_SIZE
  local xd, carry, d
  repeat
    -- single word division
    carry = 0
    xiszero = true
    for i=size,1,-1 do
      carry = carry | x[i]
      d, xd = carry // basepow, carry % basepow
      if xiszero and d ~= 0 then
        size = i
        xiszero = false
      end
      x[i] = d
      carry = xd << BINT_WORDBITS
    end
    -- digit division
    for _=1,step do
      xd, d = xd // base, xd % base
      if xiszero and xd == 0 and d == 0 then
        -- stop on leading zeros
        break
      end
      table_insert(ss, 1, BASE_LETTERS[d])
    end
  until xiszero
  if neg then
    table_insert(ss, 1, '-')
  end
  return table_concat(ss)
end

local function bint_assert_convert(x)
  return assert(tobint(x), 'value has not integer representation')
end

--- Convert a bint to a buffer of little-endian bytes.
-- @param x A bint or lua integer.
-- @param[opt] trim If true, zero bytes on the right are trimmed.
-- @return A buffer of bytes representing the input.
-- @raise Asserts in case input is not convertible to an integer.
function bint.tole(x, trim)
  x = bint_assert_convert(x)
  local s = BINT_LEPACKFMT:pack(table_unpack(x))
  if trim then
    s = s:gsub('\x00+$', '')
    if s == '' then
      s = '\x00'
    end
  end
  return s
end

--- Convert a bint to a buffer of big-endian bytes.
-- @param x A bint or lua integer.
-- @param[opt] trim If true, zero bytes on the left are trimmed.
-- @return A buffer of bytes representing the input.
-- @raise Asserts in case input is not convertible to an integer.
function bint.tobe(x, trim)
  x = bint_assert_convert(x)
  local s = BINT_LEPACKFMT:pack(table_unpack(x)):reverse()
  if trim then
    s = s:gsub('^\x00+', '')
    if s == '' then
      s = '\x00'
    end
  end
  return s
end

--- Check if a number is 0 considering bints.
-- @param x A bint or a lua number.
function bint.iszero(x)
  if getmetatable(x) == bint then
    for i=1,BINT_SIZE do
      if x[i] ~= 0 then
        return false
      end
    end
    return true
  end
  return x == 0
end

--- Check if a number is 1 considering bints.
-- @param x A bint or a lua number.
function bint.isone(x)
  if getmetatable(x) == bint then
    if x[1] ~= 1 then
      return false
    end
    for i=2,BINT_SIZE do
      if x[i] ~= 0 then
        return false
      end
    end
    return true
  end
  return x == 1
end

--- Check if a number is -1 considering bints.
-- @param x A bint or a lua number.
function bint.isminusone(x)
  if getmetatable(x) == bint then
    for i=1,BINT_SIZE do
      if x[i] ~= BINT_WORDMAX then
        return false
      end
    end
    return true
  end
  return x == -1
end
local bint_isminusone = bint.isminusone

--- Check if the input is a bint.
-- @param x Any lua value.
function bint.isbint(x)
  return getmetatable(x) == bint
end

--- Check if the input is a lua integer or a bint.
-- @param x Any lua value.
function bint.isintegral(x)
  return getmetatable(x) == bint or math_type(x) == 'integer'
end

--- Check if the input is a bint or a lua number.
-- @param x Any lua value.
function bint.isnumeric(x)
  return getmetatable(x) == bint or type(x) == 'number'
end

--- Get the number type of the input (bint, integer or float).
-- @param x Any lua value.
-- @return Returns "bint" for bints, "integer" for lua integers,
-- "float" from lua floats or nil otherwise.
function bint.type(x)
  if getmetatable(x) == bint then
    return 'bint'
  end
  return math_type(x)
end

--- Check if a number is negative considering bints.
-- Zero is guaranteed to never be negative for bints.
-- @param x A bint or a lua number.
function bint.isneg(x)
  if getmetatable(x) == bint then
    return x[BINT_SIZE] & BINT_WORDMSB ~= 0
  end
  return x < 0
end
local bint_isneg = bint.isneg

--- Check if a number is positive considering bints.
-- @param x A bint or a lua number.
function bint.ispos(x)
  if getmetatable(x) == bint then
    return not x:isneg() and not x:iszero()
  end
  return x > 0
end

--- Check if a number is even considering bints.
-- @param x A bint or a lua number.
function bint.iseven(x)
  if getmetatable(x) == bint then
    return x[1] & 1 == 0
  end
  return math_abs(x) % 2 == 0
end

--- Check if a number is odd considering bints.
-- @param x A bint or a lua number.
function bint.isodd(x)
  if getmetatable(x) == bint then
    return x[1] & 1 == 1
  end
  return math_abs(x) % 2 == 1
end

--- Create a new bint with the maximum possible integer value.
function bint.maxinteger()
  local x = setmetatable({}, bint)
  for i=1,BINT_SIZE-1 do
    x[i] = BINT_WORDMAX
  end
  x[BINT_SIZE] = BINT_WORDMAX ~ BINT_WORDMSB
  return x
end

--- Create a new bint with the minimum possible integer value.
function bint.mininteger()
  local x = setmetatable({}, bint)
  for i=1,BINT_SIZE-1 do
    x[i] = 0
  end
  x[BINT_SIZE] = BINT_WORDMSB
  return x
end

--- Bitwise left shift a bint in one bit (in-place).
function bint:_shlone()
  local wordbitsm1 = BINT_WORDBITS - 1
  for i=BINT_SIZE,2,-1 do
    self[i] = ((self[i] << 1) | (self[i-1] >> wordbitsm1)) & BINT_WORDMAX
  end
  self[1] = (self[1] << 1) & BINT_WORDMAX
  return self
end

--- Bitwise right shift a bint in one bit (in-place).
function bint:_shrone()
  local wordbitsm1 = BINT_WORDBITS - 1
  for i=1,BINT_SIZE-1 do
    self[i] = ((self[i] >> 1) | (self[i+1] << wordbitsm1)) & BINT_WORDMAX
  end
  self[BINT_SIZE] = self[BINT_SIZE] >> 1
  return self
end

-- Bitwise left shift words of a bint (in-place). Used only internally.
function bint:_shlwords(n)
  for i=BINT_SIZE,n+1,-1 do
    self[i] = self[i - n]
  end
  for i=1,n do
    self[i] = 0
  end
  return self
end

-- Bitwise right shift words of a bint (in-place). Used only internally.
function bint:_shrwords(n)
  if n < BINT_SIZE then
    for i=1,BINT_SIZE-n do
      self[i] = self[i + n]
    end
    for i=BINT_SIZE-n+1,BINT_SIZE do
      self[i] = 0
    end
  else
    for i=1,BINT_SIZE do
      self[i] = 0
    end
  end
  return self
end

--- Increment a bint by one (in-place).
function bint:_inc()
  for i=1,BINT_SIZE do
    local tmp = self[i]
    local v = (tmp + 1) & BINT_WORDMAX
    self[i] = v
    if v > tmp then
      break
    end
  end
  return self
end

--- Increment a number by one considering bints.
-- @param x A bint or a lua number to increment.
function bint.inc(x)
  local ix = tobint(x, true)
  if ix then
    return ix:_inc()
  end
  return x + 1
end

--- Decrement a bint by one (in-place).
function bint:_dec()
  for i=1,BINT_SIZE do
    local tmp = self[i]
    local v = (tmp - 1) & BINT_WORDMAX
    self[i] = v
    if v <= tmp then
      break
    end
  end
  return self
end

--- Decrement a number by one considering bints.
-- @param x A bint or a lua number to decrement.
function bint.dec(x)
  local ix = tobint(x, true)
  if ix then
    return ix:_dec()
  end
  return x - 1
end

--- Assign a bint to a new value (in-place).
-- @param y A value to be copied from.
-- @raise Asserts in case inputs are not convertible to integers.
function bint:_assign(y)
  y = bint_assert_convert(y)
  for i=1,BINT_SIZE do
    self[i] = y[i]
  end
  return self
end

--- Take absolute of a bint (in-place).
function bint:_abs()
  if self:isneg() then
    self:_unm()
  end
  return self
end

--- Take absolute of a number considering bints.
-- @param x A bint or a lua number to take the absolute.
function bint.abs(x)
  local ix = tobint(x, true)
  if ix then
    return ix:_abs()
  end
  return math_abs(x)
end
local bint_abs = bint.abs

--- Take the floor of a number considering bints.
-- @param x A bint or a lua number to perform the floor operation.
function bint.floor(x)
  if getmetatable(x) == bint then
    return bint_new(x)
  end
  return bint_new(math_floor(tonumber(x)))
end

--- Take ceil of a number considering bints.
-- @param x A bint or a lua number to perform the ceil operation.
function bint.ceil(x)
  if getmetatable(x) == bint then
    return bint_new(x)
  end
  return bint_new(math_ceil(tonumber(x)))
end

--- Wrap around bits of an integer (discarding left bits) considering bints.
-- @param x A bint or a lua integer.
-- @param y Number of right bits to preserve.
function bint.bwrap(x, y)
  x = bint_assert_convert(x)
  if y <= 0 then
    return bint_zero()
  elseif y < BINT_BITS then
    return x & (bint_one() << y):_dec()
  end
  return bint_new(x)
end

--- Rotate left integer x by y bits considering bints.
-- @param x A bint or a lua integer.
-- @param y Number of bits to rotate.
function bint.brol(x, y)
  x, y = bint_assert_convert(x), bint_assert_tointeger(y)
  if y > 0 then
    return (x << y) | (x >> (BINT_BITS - y))
  elseif y < 0 then
    if y ~= math_mininteger then
      return x:bror(-y)
    else
      x:bror(-(y+1))
      x:bror(1)
    end
  end
  return x
end

--- Rotate right integer x by y bits considering bints.
-- @param x A bint or a lua integer.
-- @param y Number of bits to rotate.
function bint.bror(x, y)
  x, y = bint_assert_convert(x), bint_assert_tointeger(y)
  if y > 0 then
    return (x >> y) | (x << (BINT_BITS - y))
  elseif y < 0 then
    if y ~= math_mininteger then
      return x:brol(-y)
    else
      x:brol(-(y+1))
      x:brol(1)
    end
  end
  return x
end

--- Truncate a number to a bint.
-- Floats numbers are truncated, that is, the fractional port is discarded.
-- @param x A number to truncate.
-- @return A new bint or nil in case the input does not fit in a bint or is not a number.
function bint.trunc(x)
  if getmetatable(x) ~= bint then
    x = tonumber(x)
    if x then
      local ty = math_type(x)
      if ty == 'float' then
        -- truncate to integer
        x = math_modf(x)
      end
      return bint_frominteger(x)
    end
    return
  end
  return bint_new(x)
end

--- Take maximum between two numbers considering bints.
-- @param x A bint or lua number to compare.
-- @param y A bint or lua number to compare.
-- @return A bint or a lua number. Guarantees to return a new bint for integer values.
function bint.max(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    return bint_new(ix > iy and ix or iy)
  end
  return bint_parse(math_max(x, y))
end

--- Take minimum between two numbers considering bints.
-- @param x A bint or lua number to compare.
-- @param y A bint or lua number to compare.
-- @return A bint or a lua number. Guarantees to return a new bint for integer values.
function bint.min(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    return bint_new(ix < iy and ix or iy)
  end
  return bint_parse(math_min(x, y))
end

--- Add an integer to a bint (in-place).
-- @param y An integer to be added.
-- @raise Asserts in case inputs are not convertible to integers.
function bint:_add(y)
  y = bint_assert_convert(y)
  local carry = 0
  for i=1,BINT_SIZE do
    local tmp = self[i] + y[i] + carry
    carry = tmp >> BINT_WORDBITS
    self[i] = tmp & BINT_WORDMAX
  end
  return self
end

--- Add two numbers considering bints.
-- @param x A bint or a lua number to be added.
-- @param y A bint or a lua number to be added.
function bint.__add(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    local z = setmetatable({}, bint)
    local carry = 0
    for i=1,BINT_SIZE do
      local tmp = ix[i] + iy[i] + carry
      carry = tmp >> BINT_WORDBITS
      z[i] = tmp & BINT_WORDMAX
    end
    return z
  end
  return bint_tonumber(x) + bint_tonumber(y)
end

--- Subtract an integer from a bint (in-place).
-- @param y An integer to subtract.
-- @raise Asserts in case inputs are not convertible to integers.
function bint:_sub(y)
  y = bint_assert_convert(y)
  local borrow = 0
  local wordmaxp1 = BINT_WORDMAX + 1
  for i=1,BINT_SIZE do
    local res = self[i] + wordmaxp1 - y[i] - borrow
    self[i] = res & BINT_WORDMAX
    borrow = (res >> BINT_WORDBITS) ~ 1
  end
  return self
end

--- Subtract two numbers considering bints.
-- @param x A bint or a lua number to be subtracted from.
-- @param y A bint or a lua number to subtract.
function bint.__sub(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    local z = setmetatable({}, bint)
    local borrow = 0
    local wordmaxp1 = BINT_WORDMAX + 1
    for i=1,BINT_SIZE do
      local res = ix[i] + wordmaxp1 - iy[i] - borrow
      z[i] = res & BINT_WORDMAX
      borrow = (res >> BINT_WORDBITS) ~ 1
    end
    return z
  end
  return bint_tonumber(x) - bint_tonumber(y)
end

--- Multiply two numbers considering bints.
-- @param x A bint or a lua number to multiply.
-- @param y A bint or a lua number to multiply.
function bint.__mul(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    local z = bint_zero()
    local sizep1 = BINT_SIZE+1
    local s = sizep1
    local e = 0
    for i=1,BINT_SIZE do
      if ix[i] ~= 0 or iy[i] ~= 0 then
        e = math_max(e, i)
        s = math_min(s, i)
      end
    end
    for i=s,e do
      for j=s,math_min(sizep1-i,e) do
        local a = ix[i] * iy[j]
        if a ~= 0 then
          local carry = 0
          for k=i+j-1,BINT_SIZE do
            local tmp = z[k] + (a & BINT_WORDMAX) + carry
            carry = tmp >> BINT_WORDBITS
            z[k] = tmp & BINT_WORDMAX
            a = a >> BINT_WORDBITS
          end
        end
      end
    end
    return z
  end
  return bint_tonumber(x) * bint_tonumber(y)
end

--- Check if bints are equal.
-- @param x A bint to compare.
-- @param y A bint to compare.
function bint.__eq(x, y)
  for i=1,BINT_SIZE do
    if x[i] ~= y[i] then
      return false
    end
  end
  return true
end

--- Check if numbers are equal considering bints.
-- @param x A bint or lua number to compare.
-- @param y A bint or lua number to compare.
function bint.eq(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    return ix == iy
  end
  return x == y
end
local bint_eq = bint.eq

local function findleftbit(x)
  for i=BINT_SIZE,1,-1 do
    local v = x[i]
    if v ~= 0 then
      local j = 0
      repeat
        v = v >> 1
        j = j + 1
      until v == 0
      return (i-1)*BINT_WORDBITS + j - 1, i
    end
  end
end

-- Single word division modulus
local function sudivmod(nume, deno)
  local rema
  local carry = 0
  for i=BINT_SIZE,1,-1 do
    carry = carry | nume[i]
    nume[i] = carry // deno
    rema = carry % deno
    carry = rema << BINT_WORDBITS
  end
  return rema
end

--- Perform unsigned division and modulo operation between two integers considering bints.
-- This is effectively the same of @{bint.udiv} and @{bint.umod}.
-- @param x The numerator, must be a bint or a lua integer.
-- @param y The denominator, must be a bint or a lua integer.
-- @return The quotient following the remainder, both bints.
-- @raise Asserts on attempt to divide by zero
-- or if inputs are not convertible to integers.
-- @see bint.udiv
-- @see bint.umod
function bint.udivmod(x, y)
  local nume = bint_new(x)
  local deno = bint_assert_convert(y)
  -- compute if high bits of denominator are all zeros
  local ishighzero = true
  for i=2,BINT_SIZE do
    if deno[i] ~= 0 then
      ishighzero = false
      break
    end
  end
  if ishighzero then
    -- try to divide by a single word (optimization)
    local low = deno[1]
    assert(low ~= 0, 'attempt to divide by zero')
    if low == 1 then
      -- denominator is one
      return nume, bint_zero()
    elseif low <= (BINT_WORDMSB - 1) then
      -- can do single word division
      local rema = sudivmod(nume, low)
      return nume, bint_fromuinteger(rema)
    end
  end
  if nume:ult(deno) then
    -- denominator is greater than numerator
    return bint_zero(), nume
  end
  -- align leftmost digits in numerator and denominator
  local denolbit = findleftbit(deno)
  local numelbit, numesize = findleftbit(nume)
  local bit = numelbit - denolbit
  deno = deno << bit
  local wordmaxp1 = BINT_WORDMAX + 1
  local wordbitsm1 = BINT_WORDBITS - 1
  local denosize = numesize
  local quot = bint_zero()
  while bit >= 0 do
    -- compute denominator <= numerator
    local le = true
    local size = math_max(numesize, denosize)
    for i=size,1,-1 do
      local a, b = deno[i], nume[i]
      if a ~= b then
        le = a < b
        break
      end
    end
    -- if the portion of the numerator above the denominator is greater or equal than to the denominator
    if le then
      -- subtract denominator from the portion of the numerator
      local borrow = 0
      for i=1,size do
        local res = nume[i] + wordmaxp1 - deno[i] - borrow
        nume[i] = res & BINT_WORDMAX
        borrow = (res >> BINT_WORDBITS) ~ 1
      end
      -- concatenate 1 to the right bit of the quotient
      local i = (bit // BINT_WORDBITS) + 1
      quot[i] = quot[i] | (1 << (bit % BINT_WORDBITS))
    end
    -- shift right the denominator in one bit
    for i=1,denosize-1 do
      deno[i] = ((deno[i] >> 1) | (deno[i+1] << wordbitsm1)) & BINT_WORDMAX
    end
    local lastdenoword = deno[denosize] >> 1
    deno[denosize] = lastdenoword
    -- recalculate denominator size (optimization)
    if lastdenoword == 0 then
      while deno[denosize] == 0 do
        denosize = denosize - 1
      end
      if denosize == 0 then
        break
      end
    end
    -- decrement current set bit for the quotient
    bit = bit - 1
  end
  -- the remaining numerator is the remainder
  return quot, nume
end
local bint_udivmod = bint.udivmod

--- Perform unsigned division between two integers considering bints.
-- @param x The numerator, must be a bint or a lua integer.
-- @param y The denominator, must be a bint or a lua integer.
-- @return The quotient, a bint.
-- @raise Asserts on attempt to divide by zero
-- or if inputs are not convertible to integers.
function bint.udiv(x, y)
  return (bint_udivmod(x, y))
end

--- Perform unsigned integer modulo operation between two integers considering bints.
-- @param x The numerator, must be a bint or a lua integer.
-- @param y The denominator, must be a bint or a lua integer.
-- @return The remainder, a bint.
-- @raise Asserts on attempt to divide by zero
-- or if the inputs are not convertible to integers.
function bint.umod(x, y)
  local _, rema = bint_udivmod(x, y)
  return rema
end
local bint_umod = bint.umod

--- Perform integer truncate division and modulo operation between two numbers considering bints.
-- This is effectively the same of @{bint.tdiv} and @{bint.tmod}.
-- @param x The numerator, a bint or lua number.
-- @param y The denominator, a bint or lua number.
-- @return The quotient following the remainder, both bint or lua number.
-- @raise Asserts on attempt to divide by zero or on division overflow.
-- @see bint.tdiv
-- @see bint.tmod
function bint.tdivmod(x, y)
  local ax, ay = bint_abs(x), bint_abs(y)
  local ix, iy = tobint(ax), tobint(ay)
  local quot, rema
  if ix and iy then
    assert(not (bint_eq(x, BINT_MININTEGER) and bint_isminusone(y)), 'division overflow')
    quot, rema = bint_udivmod(ix, iy)
  else
    quot, rema = ax // ay, ax % ay
  end
  local isxneg, isyneg = bint_isneg(x), bint_isneg(y)
  if isxneg ~= isyneg then
    quot = -quot
  end
  if isxneg then
    rema = -rema
  end
  return quot, rema
end
local bint_tdivmod = bint.tdivmod

--- Perform truncate division between two numbers considering bints.
-- Truncate division is a division that rounds the quotient towards zero.
-- @param x The numerator, a bint or lua number.
-- @param y The denominator, a bint or lua number.
-- @return The quotient, a bint or lua number.
-- @raise Asserts on attempt to divide by zero or on division overflow.
function bint.tdiv(x, y)
  return (bint_tdivmod(x, y))
end

--- Perform integer truncate modulo operation between two numbers considering bints.
-- The operation is defined as the remainder of the truncate division
-- (division that rounds the quotient towards zero).
-- @param x The numerator, a bint or lua number.
-- @param y The denominator, a bint or lua number.
-- @return The remainder, a bint or lua number.
-- @raise Asserts on attempt to divide by zero or on division overflow.
function bint.tmod(x, y)
  local _, rema = bint_tdivmod(x, y)
  return rema
end

--- Perform integer floor division and modulo operation between two numbers considering bints.
-- This is effectively the same of @{bint.__idiv} and @{bint.__mod}.
-- @param x The numerator, a bint or lua number.
-- @param y The denominator, a bint or lua number.
-- @return The quotient following the remainder, both bint or lua number.
-- @raise Asserts on attempt to divide by zero.
-- @see bint.__idiv
-- @see bint.__mod
function bint.idivmod(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    local isnumeneg = ix[BINT_SIZE] & BINT_WORDMSB ~= 0
    local isdenoneg = iy[BINT_SIZE] & BINT_WORDMSB ~= 0
    if isnumeneg then
      ix = -ix
    end
    if isdenoneg then
      iy = -iy
    end
    local quot, rema = bint_udivmod(ix, iy)
    if isnumeneg ~= isdenoneg then
      quot:_unm()
      -- round quotient towards minus infinity
      if not rema:iszero() then
        quot:_dec()
        -- adjust the remainder
        if isnumeneg and not isdenoneg then
          rema:_unm():_add(y)
        elseif isdenoneg and not isnumeneg then
          rema:_add(y)
        end
      end
    elseif isnumeneg then
      -- adjust the remainder
      rema:_unm()
    end
    return quot, rema
  end
  local nx, ny = bint_tonumber(x), bint_tonumber(y)
  return nx // ny, nx % ny
end
local bint_idivmod = bint.idivmod

--- Perform floor division between two numbers considering bints.
-- Floor division is a division that rounds the quotient towards minus infinity,
-- resulting in the floor of the division of its operands.
-- @param x The numerator, a bint or lua number.
-- @param y The denominator, a bint or lua number.
-- @return The quotient, a bint or lua number.
-- @raise Asserts on attempt to divide by zero.
function bint.__idiv(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    local isnumeneg = ix[BINT_SIZE] & BINT_WORDMSB ~= 0
    local isdenoneg = iy[BINT_SIZE] & BINT_WORDMSB ~= 0
    if isnumeneg then
      ix = -ix
    end
    if isdenoneg then
      iy = -iy
    end
    local quot, rema = bint_udivmod(ix, iy)
    if isnumeneg ~= isdenoneg then
      quot:_unm()
      -- round quotient towards minus infinity
      if not rema:iszero() then
        quot:_dec()
      end
    end
    return quot, rema
  end
  return bint_tonumber(x) // bint_tonumber(y)
end

--- Perform division between two numbers considering bints.
-- This always casts inputs to floats, for integer division only use @{bint.__idiv}.
-- @param x The numerator, a bint or lua number.
-- @param y The denominator, a bint or lua number.
-- @return The quotient, a lua number.
function bint.__div(x, y)
  return bint_tonumber(x) / bint_tonumber(y)
end

--- Perform integer floor modulo operation between two numbers considering bints.
-- The operation is defined as the remainder of the floor division
-- (division that rounds the quotient towards minus infinity).
-- @param x The numerator, a bint or lua number.
-- @param y The denominator, a bint or lua number.
-- @return The remainder, a bint or lua number.
-- @raise Asserts on attempt to divide by zero.
function bint.__mod(x, y)
  local _, rema = bint_idivmod(x, y)
  return rema
end

--- Perform integer power between two integers considering bints.
-- If y is negative then pow is performed as an unsigned integer.
-- @param x The base, an integer.
-- @param y The exponent, an integer.
-- @return The result of the pow operation, a bint.
-- @raise Asserts in case inputs are not convertible to integers.
-- @see bint.__pow
-- @see bint.upowmod
function bint.ipow(x, y)
  y = bint_assert_convert(y)
  if y:iszero() then
    return bint_one()
  elseif y:isone() then
    return bint_new(x)
  end
  -- compute exponentiation by squaring
  x, y = bint_new(x),  bint_new(y)
  local z = bint_one()
  repeat
    if y:iseven() then
      x = x * x
      y:_shrone()
    else
      z = x * z
      x = x * x
      y:_dec():_shrone()
    end
  until y:isone()
  return x * z
end

--- Perform integer power between two unsigned integers over a modulus considering bints.
-- @param x The base, an integer.
-- @param y The exponent, an integer.
-- @param m The modulus, an integer.
-- @return The result of the pow operation, a bint.
-- @raise Asserts in case inputs are not convertible to integers.
-- @see bint.__pow
-- @see bint.ipow
function bint.upowmod(x, y, m)
  m = bint_assert_convert(m)
  if m:isone() then
    return bint_zero()
  end
  x, y = bint_new(x),  bint_new(y)
  local z = bint_one()
  x = bint_umod(x, m)
  while not y:iszero() do
    if y:isodd() then
      z = bint_umod(z*x, m)
    end
    y:_shrone()
    x = bint_umod(x*x, m)
  end
  return z
end

--- Perform numeric power between two numbers considering bints.
-- This always casts inputs to floats, for integer power only use @{bint.ipow}.
-- @param x The base, a bint or lua number.
-- @param y The exponent, a bint or lua number.
-- @return The result of the pow operation, a lua number.
-- @see bint.ipow
function bint.__pow(x, y)
  return bint_tonumber(x) ^ bint_tonumber(y)
end

--- Bitwise left shift integers considering bints.
-- @param x An integer to perform the bitwise shift.
-- @param y An integer with the number of bits to shift.
-- @return The result of shift operation, a bint.
-- @raise Asserts in case inputs are not convertible to integers.
function bint.__shl(x, y)
  x, y = bint_new(x), bint_assert_tointeger(y)
  if y == math_mininteger or math_abs(y) >= BINT_BITS then
    return bint_zero()
  end
  if y < 0 then
    return x >> -y
  end
  local nvals = y // BINT_WORDBITS
  if nvals ~= 0 then
    x:_shlwords(nvals)
    y = y - nvals * BINT_WORDBITS
  end
  if y ~= 0 then
    local wordbitsmy = BINT_WORDBITS - y
    for i=BINT_SIZE,2,-1 do
      x[i] = ((x[i] << y) | (x[i-1] >> wordbitsmy)) & BINT_WORDMAX
    end
    x[1] = (x[1] << y) & BINT_WORDMAX
  end
  return x
end

--- Bitwise right shift integers considering bints.
-- @param x An integer to perform the bitwise shift.
-- @param y An integer with the number of bits to shift.
-- @return The result of shift operation, a bint.
-- @raise Asserts in case inputs are not convertible to integers.
function bint.__shr(x, y)
  x, y = bint_new(x), bint_assert_tointeger(y)
  if y == math_mininteger or math_abs(y) >= BINT_BITS then
    return bint_zero()
  end
  if y < 0 then
    return x << -y
  end
  local nvals = y // BINT_WORDBITS
  if nvals ~= 0 then
    x:_shrwords(nvals)
    y = y - nvals * BINT_WORDBITS
  end
  if y ~= 0 then
    local wordbitsmy = BINT_WORDBITS - y
    for i=1,BINT_SIZE-1 do
      x[i] = ((x[i] >> y) | (x[i+1] << wordbitsmy)) & BINT_WORDMAX
    end
    x[BINT_SIZE] = x[BINT_SIZE] >> y
  end
  return x
end

--- Bitwise AND bints (in-place).
-- @param y An integer to perform bitwise AND.
-- @raise Asserts in case inputs are not convertible to integers.
function bint:_band(y)
  y = bint_assert_convert(y)
  for i=1,BINT_SIZE do
    self[i] = self[i] & y[i]
  end
  return self
end

--- Bitwise AND two integers considering bints.
-- @param x An integer to perform bitwise AND.
-- @param y An integer to perform bitwise AND.
-- @raise Asserts in case inputs are not convertible to integers.
function bint.__band(x, y)
  return bint_new(x):_band(y)
end

--- Bitwise OR bints (in-place).
-- @param y An integer to perform bitwise OR.
-- @raise Asserts in case inputs are not convertible to integers.
function bint:_bor(y)
  y = bint_assert_convert(y)
  for i=1,BINT_SIZE do
    self[i] = self[i] | y[i]
  end
  return self
end

--- Bitwise OR two integers considering bints.
-- @param x An integer to perform bitwise OR.
-- @param y An integer to perform bitwise OR.
-- @raise Asserts in case inputs are not convertible to integers.
function bint.__bor(x, y)
  return bint_new(x):_bor(y)
end

--- Bitwise XOR bints (in-place).
-- @param y An integer to perform bitwise XOR.
-- @raise Asserts in case inputs are not convertible to integers.
function bint:_bxor(y)
  y = bint_assert_convert(y)
  for i=1,BINT_SIZE do
    self[i] = self[i] ~ y[i]
  end
  return self
end

--- Bitwise XOR two integers considering bints.
-- @param x An integer to perform bitwise XOR.
-- @param y An integer to perform bitwise XOR.
-- @raise Asserts in case inputs are not convertible to integers.
function bint.__bxor(x, y)
  return bint_new(x):_bxor(y)
end

--- Bitwise NOT a bint (in-place).
function bint:_bnot()
  for i=1,BINT_SIZE do
    self[i] = (~self[i]) & BINT_WORDMAX
  end
  return self
end

--- Bitwise NOT a bint.
-- @param x An integer to perform bitwise NOT.
-- @raise Asserts in case inputs are not convertible to integers.
function bint.__bnot(x)
  local y = setmetatable({}, bint)
  for i=1,BINT_SIZE do
    y[i] = (~x[i]) & BINT_WORDMAX
  end
  return y
end

--- Negate a bint (in-place). This effectively applies two's complements.
function bint:_unm()
  return self:_bnot():_inc()
end

--- Negate a bint. This effectively applies two's complements.
-- @param x A bint to perform negation.
function bint.__unm(x)
  return (~x):_inc()
end

--- Compare if integer x is less than y considering bints (unsigned version).
-- @param x Left integer to compare.
-- @param y Right integer to compare.
-- @raise Asserts in case inputs are not convertible to integers.
-- @see bint.__lt
function bint.ult(x, y)
  x, y = bint_assert_convert(x), bint_assert_convert(y)
  for i=BINT_SIZE,1,-1 do
    local a, b = x[i], y[i]
    if a ~= b then
      return a < b
    end
  end
  return false
end

--- Compare if bint x is less or equal than y considering bints (unsigned version).
-- @param x Left integer to compare.
-- @param y Right integer to compare.
-- @raise Asserts in case inputs are not convertible to integers.
-- @see bint.__le
function bint.ule(x, y)
  x, y = bint_assert_convert(x), bint_assert_convert(y)
  for i=BINT_SIZE,1,-1 do
    local a, b = x[i], y[i]
    if a ~= b then
      return a < b
    end
  end
  return true
end

--- Compare if number x is less than y considering bints and signs.
-- @param x Left value to compare, a bint or lua number.
-- @param y Right value to compare, a bint or lua number.
-- @see bint.ult
function bint.__lt(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    local xneg = ix[BINT_SIZE] & BINT_WORDMSB ~= 0
    local yneg = iy[BINT_SIZE] & BINT_WORDMSB ~= 0
    if xneg == yneg then
      for i=BINT_SIZE,1,-1 do
        local a, b = ix[i], iy[i]
        if a ~= b then
          return a < b
        end
      end
      return false
    end
    return xneg and not yneg
  end
  return bint_tonumber(x) < bint_tonumber(y)
end

--- Compare if number x is less or equal than y considering bints and signs.
-- @param x Left value to compare, a bint or lua number.
-- @param y Right value to compare, a bint or lua number.
-- @see bint.ule
function bint.__le(x, y)
  local ix, iy = tobint(x), tobint(y)
  if ix and iy then
    local xneg = ix[BINT_SIZE] & BINT_WORDMSB ~= 0
    local yneg = iy[BINT_SIZE] & BINT_WORDMSB ~= 0
    if xneg == yneg then
      for i=BINT_SIZE,1,-1 do
        local a, b = ix[i], iy[i]
        if a ~= b then
          return a < b
        end
      end
      return true
    end
    return xneg and not yneg
  end
  return bint_tonumber(x) <= bint_tonumber(y)
end

--- Convert a bint to a string on base 10.
-- @see bint.tobase
function bint:__tostring()
  return self:tobase(10)
end

-- Allow creating bints by calling bint itself
setmetatable(bint, {
  __call = function(_, x)
    return bint_new(x)
  end
})

BINT_MATHMININTEGER, BINT_MATHMAXINTEGER = bint_new(math.mininteger), bint_new(math.maxinteger)
BINT_MININTEGER = bint.mininteger()
memo[memoindex] = bint

return bint

end

return newmodule
end
end

do
local _ENV = _ENV
package.preload[ "astropacks-aostandard.src.ao.libs" ] = function( ... ) local arg = _G.arg;
-- These libs should exist in ao

local mod = {}

---@module "json"
local mod_json;

---@module "bint"
local mod_bint;


-- Define json

local jsonstatus, json = pcall(require, "json")

if jsonstatus then
  mod_json = json
else
  if not mod_json then
    print("Library 'json' does not exist. Using fallback dkjson.")
  end

  local dkjsonstatus, dkjson = pcall(require, "dkjson")
  mod_json = dkjson
end

local bintstatus, bint = pcall(require, ".bint")

if bintstatus then
  mod_bint = bint
else
  if not mod_bint then
    print("Library '.bint' does not exist. Using fallback https://github.com/permaweb/aos/blob/main/process/bint.lua")
  end

  -- If bint is not found, then use the copied bint module from
  -- https://github.com/permaweb/aos/blob/main/process/bint.lua
  local copiedbintstatus, copiedbint = pcall(require, "astropacks-aostandard.src.ao.bint")
  mod_bint = copiedbint
end

---Create a `Bint` instance.
---@param bits? number (Optional) Number of bits for the integer representation,
---must be multiple of wordbits and at least 64. Defaults to 512.
---@param word_bits? number (Optional) Number of the bits for the internal word,
--- defaults to half of Lua's integer size.
---@returns Bint
mod.bint = function(bits, word_bits)

  if not bits or type(bits) ~= "number" then
    bits = 512
  end

  local instance = mod_bint(bits, word_bits)

  return instance
end

mod.json = mod_json

return mod
end
end

do
local _ENV = _ENV
package.preload[ "astropacks-aostandard.src.assertions.mod" ] = function( ... ) local arg = _G.arg;
local runtime = require "astropacks-aostandard.src.runtime.mod"

local mod = {}

local function msg_to_str(msg)
  if type(msg) ~= "string" then
    msg = ""
  end

  return msg
end

---Assert that the given value is a `Bint`.
---@param value? Bint
---@param msg? string
function mod.is_bint(value, msg)
  msg = msg_to_str(msg)

  local hasBintFields = true

  if value ~= nil then
    if not type(value) == "table" then
      hasBintFields = false
    end

    if not value.tobase then
      hasBintFields = false
    end

    if not value.__le then
      hasBintFields = false
    end

    if not value.__lt then
      hasBintFields = false
    end
  else
    hasBintFields = false
  end

  if not hasBintFields then
    runtime.throw(msg)
  end
end

---Assert that the given values are not the same.
---@param a any
---@param b any
---@param msg? string
function mod.is_not_same(a, b, msg)
  msg = msg_to_str(msg)

  if a == b then
    runtime.throw(msg)
  end
end

---Assert that the given value is not `nil`?
---@param v any
---@param msg any
function mod.is_not_nil(v, msg)
  msg = msg_to_str(msg)

  if v == nil then
    runtime.throw(msg)
  end
end

return mod
end
end

do
local _ENV = _ENV
package.preload[ "astropacks-aostandard.src.configs.validator" ] = function( ... ) local arg = _G.arg;
local schema = require "astropacks-aostandard.src.schema.mod"

local mod = {}

---Run the configs against the `Schema` validator, but capture the error and
---include outputting that the configs are invalid.
---
---@param configs table
---@param shape table
function mod.validate(configs, shape)
  local _, err = pcall(function()
    schema.validate(configs, shape)
  end)

  if type(err) == "nil" then
    return
  end

  if err then
    error("\n\n[ CONFIGS ERROR ]\n\n" .. err, 2)
  end
end

return mod
end
end

do
local _ENV = _ENV
package.preload[ "astropacks-aostandard.src.extensions.mod" ] = function( ... ) local arg = _G.arg;
local tables = require "astropacks-aostandard.src.extensions.tables"

local mod = {
  tables = tables
}

return mod
end
end

do
local _ENV = _ENV
package.preload[ "astropacks-aostandard.src.extensions.tables" ] = function( ... ) local arg = _G.arg;
local mod = {}

-- Extending the STD tables

-- Given a list and a filter function, it returns
-- the only the values of the table for which the
-- filter function return true
---@param list table
---@param filter_fn fun(val: unknown): boolean
---@return table
function mod.filter(list, filter_fn)
  local filtered = {}

  for _, value in pairs(list) do
    if filter_fn(value) then
      table.insert(filtered, value)
    end
  end

  return filtered
end

-- Given a list it returns its keys
---@param list table
---@return table
function mod.keys(list)
  local keys = {}

  for k in pairs(list) do
    table.insert(keys, k)
  end

  return keys
end

-- Given a list it returns its values
---@param list table
---@return table
function mod.values(list)
  local ret = {}

  for _, v in pairs(list) do
    table.insert(ret, v)
  end

  return ret
end

-- Given a list, a reducer function and an optional
-- initial value, it executes the reducer, which
-- handles the currently iterated value, as well
-- as the result of the previous reducer calculation
---@param list table
---@param reducer_fn fun(accumulator: unknown, currentValue: unknown, index: unknown): unknown
---@param initialValue? unknown
---@return unknown
function mod.reduce(list, reducer_fn, initialValue)
  local accumulator = initialValue or 0

  for k, v in ipairs(list) do
    if k == 1 and not initialValue then
      accumulator = v
    else
      accumulator = reducer_fn(accumulator, v, k)
    end
  end

  return accumulator
end

---Applies `fn` to each element of `list` and returns a new table with the results.
---@param list table Input table.
---@param fn function Function to apply to each element.
---@return table `New table with transformed elements.`a
function mod.map(list, fn)
  assert(type(list) == "table", "first argument should be an Array")
  assert(type(fn) == "function", "second argument should be a unary function")

  local function map(result, v, k)
    result[k] = fn(v, k)
    return result
  end

  return mod.reduce(list, map, {})
end

-- Given a finder function, it returns the value
-- of the first table element for which its value
-- matches the finder function result or nil if
-- nothing matched
---@param list table
---@param find_fn fun(val: unknown): boolean
---@return unknown|nil
function mod.find(list, find_fn)
  for _, v in ipairs(list) do
    if find_fn(v) then
      return v
    end
  end

  return nil
end

-- Given a value, it returns if a table contains
-- the mentioned value
---@param list table
---@param val unknown
---@return boolean
function mod.includes(list, val)
  for _, v in pairs(list) do
    if v == val then return true end
  end

  return false
end

-- Given a table it prints out its content
---@param list table
---@param indentation number?
function mod.print(list, indentation)
  if indentation == nil then indentation = 0 end

  if indentation == 0 then
    io.write(string.rep(" ", indentation) .. "{\n")
  end

  for k, v in pairs(list or {}) do
    io.write(
      string.rep(" ", indentation + 2) ..
      tostring(k) ..
      " = "
    )

    if type(v) == "table" then
      if #mod.keys(v) == 0 then
        io.write("{}\n")
      else
        io.write("{\n")
        mod.print(v, indentation + 2)
      end
    else io.write(tostring(v) .. "\n") end
  end

  io.write(string.rep(" ", indentation) .. "}\n")
end

---Merges tables, overwriting values for duplicate keys.
---@param ... table Tables to merge.
---@return table `Merged table.`
function mod.merge_objects(...)
  local result = {}

  -- Iterate over each table passed as an argument
  for _, t in ipairs({ ... }) do
    for k, v in pairs(t) do
      -- Directly assign the value; this will overwrite for dictionary keys if necessary
      result[k] = v
    end
  end

  return result
end

---Create a copy of a table. The resulting table and any nested tables will have
---new references.
---
---@param t table Table to make a copy of. It should not contain functions.
---@return table A copy of the table.
function mod.copy_of(t)

  local result = {}

  for k, v in pairs(t) do
    -- These values should not be copied over because we cannot guarantee they
    -- will have new references
    if type(v) == "function" then
      error("Cannot create copy of table function", 2)
    end

    if type(v) == "thread" then
      error("Cannot create copy of table thread", 2)
    end

    if type(v) == "userdata" then
      error("Cannot create copy of table thread", 2)
    end

    -- All nested tables need to be copied as well
    if type(v) == "table" then
      result[k] = mod.copy_of(v)
    else
      result[k] = v
    end
  end

  return result
end

---Taken from https://www.lua.org/pil/19.3.html
function mod.pairs_by_keys(t, f)
  local a = {}
  for n in pairs(t) do table.insert(a, n) end
  table.sort(a, f)
  local i = 0      -- iterator variable
  local iter = function ()   -- iterator function
    i = i + 1
    if a[i] == nil then return nil
    else return a[i], t[a[i]]
    end
  end
  return iter
end

function mod.sort(t)
  if not t then
    return {}
  end

  if type(t) ~= "table" then
    error("Cannot sort non-table value", 2)
  end

  local ret = {}

  for i, v in mod.pairs_by_keys(t) do
    ret[i] = v
  end

  return ret
end

return mod
end
end

do
local _ENV = _ENV
package.preload[ "astropacks-aostandard.src.output.mod" ] = function( ... ) local arg = _G.arg;
local aolibs = require "astropacks-aostandard.src.ao.libs"

local json = aolibs.json

local mod = {}

---Output a JSON string.
---@param input any The input to output as a JSON string.
mod.json = function(input)
  print(json.encode(input))
end

---Output plain text. This converts non-strings to strings.
---@param input any
mod.text = function(input)
  if type(input) ~= "string" then
    input = tostring(input)
  end
  
  print(input)
end

return mod
end
end

do
local _ENV = _ENV
package.preload[ "astropacks-aostandard.src.runtime.mod" ] = function( ... ) local arg = _G.arg;
local mod = {}

---Throw an error object with a default level of `2`.
---@param message any
---@param level? number
function mod.throw(message, level)
  if level and type(level) == "number" then
    level = level + 1
  else
    level = 3
  end

  -- Error messages should only come in as strings, but in case they are not ...
  if type(message) ~="string" then
    -- ... then output that a table was provided ...
    if type(message) == "table" then
      message = "<table>"
    else
      -- ... and convert non-table/string values to strings
      message = tostring(message)
    end
  end

  -- The `|message_body|` below is a stop block for string replacement. In the
  -- `src.actions.mod` module, there is code that throws throws the errors
  -- from action handlers. Before throwing the error, the error is cleaned up
  -- using the `mod.clean_error()` function below, which removes all characters
  -- up to this stop block. That way the error comes out clean without the Lua
  -- filenames provided. For example:
  --
  --     - Code calls runtime.throw("some error message")
  --     - This module turns the error message into: "|message_body|some error message"
  --     - The actions module handles the error and calls `runtime.clean_error()` on it
  --     - The error becomes ---> "some_error_message"
  --     - ... and not like  ---> "some_file.lua:11: some error message"
  --
  error({ message = "|message_body|" .. message }, level)
end

---Output a warning. This can only be enabled if `LUA_ENV_SHOW_WARNINGS=true`.
---
---@param message string The warning message write.
function mod.warn(message)
  local showWarnings = os.getenv("LUA_ENV_SHOW_WARNINGS")

  if showWarnings == "true" then
    io.write("\n\n[ WARNING ] " .. message .. "\n\n")
  end
end


---Clean up the given error assuming it was thrown using `runtime.throw()`.
---
---@param err? any
function mod.clean_error(err)
  if type(err) == "string" then
    return err
  end

  if type(err) == "table" then
    if err.message ~= nil then
      return string.gsub(err.message, ".+message_body|", "")
    end
  end

  return err
end


function mod.require(module_name)
end

return mod
end
end

do
local _ENV = _ENV
package.preload[ "astropacks-aostandard.src.schema.mod" ] = function( ... ) local arg = _G.arg;
local Type = require "arweave.types.type"
local assertions = require "astropacks-aostandard.src.assertions.mod"

local mod = {}

---Assert the given value is a `Bint`.
---
---@param value unknown
---@param errMsg string
local function assert_type_bint(value, errMsg)
  Type:custom(errMsg, function()
    local status, e = pcall(function()
      assertions.is_bint(value, errMsg)
    end)

    if not status and e then
      return false
    end

    return true

  end):assert(value)
end

---@alias TypeName "string"|"number"|"boolean"|"table"|"Bint"
---Assert the 
---
---@param typeName TypeName The type the value should be.
---@param value unknown The value to assert.
---@param errMsg string Error message to throw.
local function assert_type(typeName, value, errMsg)
  if typeName == "Bint" then
    assert_type_bint(value, errMsg)
    return
  end

  if type(typeName) == "string" and type(value) == "string" then
    local spacesRemoved = string.gsub(value, "%s+", "")
    assert(spacesRemoved ~= "", errMsg .. " (empty string provided)")
  end

  Type[typeName](Type, errMsg):assert(value)
end

---Assert the given `obj` using the given `rules`.
---
---@param objectKeyName string The key name of the object.
---@param obj table The object in question
---@param rules ObjectTypeRules The rules to use against the object.
local function assert_object_keys_and_values(objectKeyName, obj, rules)
  local expectedKeysType = rules.keys
  local expectedValuesType = rules.values

  for i, value in pairs(obj or {}) do
    -- Assert the key's type
    local keyErrMsg = "Key '" .. i .. "' in object '" .. objectKeyName .. "' must be a " .. expectedKeysType
    assert_type(expectedKeysType, keyErrMsg, i)

    -- Assert the value's type
    local valueErrMsg = "Field '" .. objectKeyName .. "['" .. i .. "']' value must be a " .. expectedValuesType
    assert_type(expectedValuesType, value, valueErrMsg)
  end
end

---@alias Schema table<string, unknown>|table<string, ObjectTypeRules>
---Run assertions on the given `obj` using the given `schema` rules.
---@param obj table object in question.
---@param schema Schema
local function assert_object_matches_schema(obj, schema)
  for key, schemaValue in pairs(schema) do

    local objectValue = obj[key]

    if type(schemaValue) == "table" then
      if schemaValue.is_object_type and type(schemaValue.object_type_rules) == "table" then
        assert_object_keys_and_values(key, objectValue, schemaValue.object_type_rules)
      else
        assert_object_matches_schema(objectValue, schemaValue)
      end
    else
      -- Assert the value's type
      local expectedType = schemaValue; -- Change name here for semantics
      local errMsg = "Field '" .. key .. "' must be a " .. expectedType
      assert_type(expectedType, objectValue, errMsg)
    end
  end
end

---@alias ObjectType { is_object_type: true, object_type_rules: ObjectTypeRules }
---@alias ObjectTypeRules { keys: "string", values: TypeName }
---Create a typing defintion for the keys and values for an object.
---
---@param object_type_rules ObjectTypeRules
---@return ObjectType
function mod.object_type(object_type_rules)
  return {
    is_object_type = true,
    object_type_rules = object_type_rules
  }
end

function mod.validate(obj, schema)
  assert(schema ~= nil, "Cannot validate schema on nil object")
  assert_object_matches_schema(obj, schema)
end

return mod
end
end

do
local _ENV = _ENV
package.preload[ "dkjson" ] = function( ... ) local arg = _G.arg;
-- Module options:
local always_use_lpeg = false
local register_global_module_table = false
local global_module_name = 'json'

--[==[

David Kolf's JSON module for Lua 5.1 - 5.4

Version 2.8


For the documentation see the corresponding readme.txt or visit
<http://dkolf.de/dkjson-lua/>.

You can contact the author by sending an e-mail to 'david' at the
domain 'dkolf.de'.


Copyright (C) 2010-2024 David Heiko Kolf

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

--]==]

-- global dependencies:
local pairs, type, tostring, tonumber, getmetatable, setmetatable =
      pairs, type, tostring, tonumber, getmetatable, setmetatable
local error, require, pcall, select = error, require, pcall, select
local floor, huge = math.floor, math.huge
local strrep, gsub, strsub, strbyte, strchar, strfind, strlen, strformat =
      string.rep, string.gsub, string.sub, string.byte, string.char,
      string.find, string.len, string.format
local strmatch = string.match
local concat = table.concat

local json = { version = "dkjson 2.8" }

local jsonlpeg = {}

if register_global_module_table then
  if always_use_lpeg then
    _G[global_module_name] = jsonlpeg
  else
    _G[global_module_name] = json
  end
end

local _ENV = nil -- blocking globals in Lua 5.2 and later

pcall (function()
  -- Enable access to blocked metatables.
  -- Don't worry, this module doesn't change anything in them.
  local debmeta = require "debug".getmetatable
  if debmeta then getmetatable = debmeta end
end)

json.null = setmetatable ({}, {
  __tojson = function () return "null" end
})

local function isarray (tbl)
  local max, n, arraylen = 0, 0, 0
  for k,v in pairs (tbl) do
    if k == 'n' and type(v) == 'number' then
      arraylen = v
      if v > max then
        max = v
      end
    else
      if type(k) ~= 'number' or k < 1 or floor(k) ~= k then
        return false
      end
      if k > max then
        max = k
      end
      n = n + 1
    end
  end
  if max > 10 and max > arraylen and max > n * 2 then
    return false -- don't create an array with too many holes
  end
  return true, max
end

local escapecodes = {
  ["\""] = "\\\"", ["\\"] = "\\\\", ["\b"] = "\\b", ["\f"] = "\\f",
  ["\n"] = "\\n",  ["\r"] = "\\r",  ["\t"] = "\\t"
}

local function escapeutf8 (uchar)
  local value = escapecodes[uchar]
  if value then
    return value
  end
  local a, b, c, d = strbyte (uchar, 1, 4)
  a, b, c, d = a or 0, b or 0, c or 0, d or 0
  if a <= 0x7f then
    value = a
  elseif 0xc0 <= a and a <= 0xdf and b >= 0x80 then
    value = (a - 0xc0) * 0x40 + b - 0x80
  elseif 0xe0 <= a and a <= 0xef and b >= 0x80 and c >= 0x80 then
    value = ((a - 0xe0) * 0x40 + b - 0x80) * 0x40 + c - 0x80
  elseif 0xf0 <= a and a <= 0xf7 and b >= 0x80 and c >= 0x80 and d >= 0x80 then
    value = (((a - 0xf0) * 0x40 + b - 0x80) * 0x40 + c - 0x80) * 0x40 + d - 0x80
  else
    return ""
  end
  if value <= 0xffff then
    return strformat ("\\u%.4x", value)
  elseif value <= 0x10ffff then
    -- encode as UTF-16 surrogate pair
    value = value - 0x10000
    local highsur, lowsur = 0xD800 + floor (value/0x400), 0xDC00 + (value % 0x400)
    return strformat ("\\u%.4x\\u%.4x", highsur, lowsur)
  else
    return ""
  end
end

local function fsub (str, pattern, repl)
  -- gsub always builds a new string in a buffer, even when no match
  -- exists. First using find should be more efficient when most strings
  -- don't contain the pattern.
  if strfind (str, pattern) then
    return gsub (str, pattern, repl)
  else
    return str
  end
end

local function quotestring (value)
  -- based on the regexp "escapable" in https://github.com/douglascrockford/JSON-js
  value = fsub (value, "[%z\1-\31\"\\\127]", escapeutf8)
  if strfind (value, "[\194\216\220\225\226\239]") then
    value = fsub (value, "\194[\128-\159\173]", escapeutf8)
    value = fsub (value, "\216[\128-\132]", escapeutf8)
    value = fsub (value, "\220\143", escapeutf8)
    value = fsub (value, "\225\158[\180\181]", escapeutf8)
    value = fsub (value, "\226\128[\140-\143\168-\175]", escapeutf8)
    value = fsub (value, "\226\129[\160-\175]", escapeutf8)
    value = fsub (value, "\239\187\191", escapeutf8)
    value = fsub (value, "\239\191[\176-\191]", escapeutf8)
  end
  return "\"" .. value .. "\""
end
json.quotestring = quotestring

local function replace(str, o, n)
  local i, j = strfind (str, o, 1, true)
  if i then
    return strsub(str, 1, i-1) .. n .. strsub(str, j+1, -1)
  else
    return str
  end
end

-- locale independent num2str and str2num functions
local decpoint, numfilter

local function updatedecpoint ()
  decpoint = strmatch(tostring(0.5), "([^05+])")
  -- build a filter that can be used to remove group separators
  numfilter = "[^0-9%-%+eE" .. gsub(decpoint, "[%^%$%(%)%%%.%[%]%*%+%-%?]", "%%%0") .. "]+"
end

updatedecpoint()

local function num2str (num)
  return replace(fsub(tostring(num), numfilter, ""), decpoint, ".")
end

local function str2num (str)
  local num = tonumber(replace(str, ".", decpoint))
  if not num then
    updatedecpoint()
    num = tonumber(replace(str, ".", decpoint))
  end
  return num
end

local function addnewline2 (level, buffer, buflen)
  buffer[buflen+1] = "\n"
  buffer[buflen+2] = strrep ("  ", level)
  buflen = buflen + 2
  return buflen
end

function json.addnewline (state)
  if state.indent then
    state.bufferlen = addnewline2 (state.level or 0,
                           state.buffer, state.bufferlen or #(state.buffer))
  end
end

local encode2 -- forward declaration

local function addpair (key, value, prev, indent, level, buffer, buflen, tables, globalorder, state)
  local kt = type (key)
  if kt ~= 'string' and kt ~= 'number' then
    return nil, "type '" .. kt .. "' is not supported as a key by JSON."
  end
  if prev then
    buflen = buflen + 1
    buffer[buflen] = ","
  end
  if indent then
    buflen = addnewline2 (level, buffer, buflen)
  end
  -- When Lua is compiled with LUA_NOCVTN2S this will fail when
  -- numbers are mixed into the keys of the table. JSON keys are always
  -- strings, so this would be an implicit conversion too and the failure
  -- is intentional.
  buffer[buflen+1] = quotestring (key)
  buffer[buflen+2] = ":"
  return encode2 (value, indent, level, buffer, buflen + 2, tables, globalorder, state)
end

local function appendcustom(res, buffer, state)
  local buflen = state.bufferlen
  if type (res) == 'string' then
    buflen = buflen + 1
    buffer[buflen] = res
  end
  return buflen
end

local function exception(reason, value, state, buffer, buflen, defaultmessage)
  defaultmessage = defaultmessage or reason
  local handler = state.exception
  if not handler then
    return nil, defaultmessage
  else
    state.bufferlen = buflen
    local ret, msg = handler (reason, value, state, defaultmessage)
    if not ret then return nil, msg or defaultmessage end
    return appendcustom(ret, buffer, state)
  end
end

function json.encodeexception(reason, value, state, defaultmessage)
  return quotestring("<" .. defaultmessage .. ">")
end

encode2 = function (value, indent, level, buffer, buflen, tables, globalorder, state)
  local valtype = type (value)
  local valmeta = getmetatable (value)
  valmeta = type (valmeta) == 'table' and valmeta -- only tables
  local valtojson = valmeta and valmeta.__tojson
  if valtojson then
    if tables[value] then
      return exception('reference cycle', value, state, buffer, buflen)
    end
    tables[value] = true
    state.bufferlen = buflen
    local ret, msg = valtojson (value, state)
    if not ret then return exception('custom encoder failed', value, state, buffer, buflen, msg) end
    tables[value] = nil
    buflen = appendcustom(ret, buffer, state)
  elseif value == nil then
    buflen = buflen + 1
    buffer[buflen] = "null"
  elseif valtype == 'number' then
    local s
    if value ~= value or value >= huge or -value >= huge then
      -- This is the behaviour of the original JSON implementation.
      s = "null"
    else
      s = num2str (value)
    end
    buflen = buflen + 1
    buffer[buflen] = s
  elseif valtype == 'boolean' then
    buflen = buflen + 1
    buffer[buflen] = value and "true" or "false"
  elseif valtype == 'string' then
    buflen = buflen + 1
    buffer[buflen] = quotestring (value)
  elseif valtype == 'table' then
    if tables[value] then
      return exception('reference cycle', value, state, buffer, buflen)
    end
    tables[value] = true
    level = level + 1
    local isa, n = isarray (value)
    if n == 0 and valmeta and valmeta.__jsontype == 'object' then
      isa = false
    end
    local msg
    if isa then -- JSON array
      buflen = buflen + 1
      buffer[buflen] = "["
      for i = 1, n do
        buflen, msg = encode2 (value[i], indent, level, buffer, buflen, tables, globalorder, state)
        if not buflen then return nil, msg end
        if i < n then
          buflen = buflen + 1
          buffer[buflen] = ","
        end
      end
      buflen = buflen + 1
      buffer[buflen] = "]"
    else -- JSON object
      local prev = false
      buflen = buflen + 1
      buffer[buflen] = "{"
      local order = valmeta and valmeta.__jsonorder or globalorder
      if order then
        local used = {}
        n = #order
        for i = 1, n do
          local k = order[i]
          local v = value[k]
          if v ~= nil then
            used[k] = true
            buflen, msg = addpair (k, v, prev, indent, level, buffer, buflen, tables, globalorder, state)
            if not buflen then return nil, msg end
            prev = true -- add a seperator before the next element
          end
        end
        for k,v in pairs (value) do
          if not used[k] then
            buflen, msg = addpair (k, v, prev, indent, level, buffer, buflen, tables, globalorder, state)
            if not buflen then return nil, msg end
            prev = true -- add a seperator before the next element
          end
        end
      else -- unordered
        for k,v in pairs (value) do
          buflen, msg = addpair (k, v, prev, indent, level, buffer, buflen, tables, globalorder, state)
          if not buflen then return nil, msg end
          prev = true -- add a seperator before the next element
        end
      end
      if indent then
        buflen = addnewline2 (level - 1, buffer, buflen)
      end
      buflen = buflen + 1
      buffer[buflen] = "}"
    end
    tables[value] = nil
  else
    return exception ('unsupported type', value, state, buffer, buflen,
      "type '" .. valtype .. "' is not supported by JSON.")
  end
  return buflen
end

function json.encode (value, state)
  state = state or {}
  local oldbuffer = state.buffer
  local buffer = oldbuffer or {}
  state.buffer = buffer
  updatedecpoint()
  local ret, msg = encode2 (value, state.indent, state.level or 0,
                   buffer, state.bufferlen or 0, state.tables or {}, state.keyorder, state)
  if not ret then
    error (msg, 2)
  elseif oldbuffer == buffer then
    state.bufferlen = ret
    return true
  else
    state.bufferlen = nil
    state.buffer = nil
    return concat (buffer)
  end
end

local function loc (str, where)
  local line, pos, linepos = 1, 1, 0
  while true do
    pos = strfind (str, "\n", pos, true)
    if pos and pos < where then
      line = line + 1
      linepos = pos
      pos = pos + 1
    else
      break
    end
  end
  return strformat ("line %d, column %d", line, where - linepos)
end

local function unterminated (str, what, where)
  return nil, strlen (str) + 1, "unterminated " .. what .. " at " .. loc (str, where)
end

local function scanwhite (str, pos)
  while true do
    pos = strfind (str, "%S", pos)
    if not pos then return nil end
    local sub2 = strsub (str, pos, pos + 1)
    if sub2 == "\239\187" and strsub (str, pos + 2, pos + 2) == "\191" then
      -- UTF-8 Byte Order Mark
      pos = pos + 3
    elseif sub2 == "//" then
      pos = strfind (str, "[\n\r]", pos + 2)
      if not pos then return nil end
    elseif sub2 == "/*" then
      pos = strfind (str, "*/", pos + 2)
      if not pos then return nil end
      pos = pos + 2
    else
      return pos
    end
  end
end

local escapechars = {
  ["\""] = "\"", ["\\"] = "\\", ["/"] = "/", ["b"] = "\b", ["f"] = "\f",
  ["n"] = "\n", ["r"] = "\r", ["t"] = "\t"
}

local function unichar (value)
  if value < 0 then
    return nil
  elseif value <= 0x007f then
    return strchar (value)
  elseif value <= 0x07ff then
    return strchar (0xc0 + floor(value/0x40),
                    0x80 + (floor(value) % 0x40))
  elseif value <= 0xffff then
    return strchar (0xe0 + floor(value/0x1000),
                    0x80 + (floor(value/0x40) % 0x40),
                    0x80 + (floor(value) % 0x40))
  elseif value <= 0x10ffff then
    return strchar (0xf0 + floor(value/0x40000),
                    0x80 + (floor(value/0x1000) % 0x40),
                    0x80 + (floor(value/0x40) % 0x40),
                    0x80 + (floor(value) % 0x40))
  else
    return nil
  end
end

local function scanstring (str, pos)
  local lastpos = pos + 1
  local buffer, n = {}, 0
  while true do
    local nextpos = strfind (str, "[\"\\]", lastpos)
    if not nextpos then
      return unterminated (str, "string", pos)
    end
    if nextpos > lastpos then
      n = n + 1
      buffer[n] = strsub (str, lastpos, nextpos - 1)
    end
    if strsub (str, nextpos, nextpos) == "\"" then
      lastpos = nextpos + 1
      break
    else
      local escchar = strsub (str, nextpos + 1, nextpos + 1)
      local value
      if escchar == "u" then
        value = tonumber (strsub (str, nextpos + 2, nextpos + 5), 16)
        if value then
          local value2
          if 0xD800 <= value and value <= 0xDBff then
            -- we have the high surrogate of UTF-16. Check if there is a
            -- low surrogate escaped nearby to combine them.
            if strsub (str, nextpos + 6, nextpos + 7) == "\\u" then
              value2 = tonumber (strsub (str, nextpos + 8, nextpos + 11), 16)
              if value2 and 0xDC00 <= value2 and value2 <= 0xDFFF then
                value = (value - 0xD800)  * 0x400 + (value2 - 0xDC00) + 0x10000
              else
                value2 = nil -- in case it was out of range for a low surrogate
              end
            end
          end
          value = value and unichar (value)
          if value then
            if value2 then
              lastpos = nextpos + 12
            else
              lastpos = nextpos + 6
            end
          end
        end
      end
      if not value then
        value = escapechars[escchar] or escchar
        lastpos = nextpos + 2
      end
      n = n + 1
      buffer[n] = value
    end
  end
  if n == 1 then
    return buffer[1], lastpos
  elseif n > 1 then
    return concat (buffer), lastpos
  else
    return "", lastpos
  end
end

local scanvalue -- forward declaration

local function scantable (what, closechar, str, startpos, nullval, objectmeta, arraymeta)
  local tbl, n = {}, 0
  local pos = startpos + 1
  if what == 'object' then
    setmetatable (tbl, objectmeta)
  else
    setmetatable (tbl, arraymeta)
  end
  while true do
    pos = scanwhite (str, pos)
    if not pos then return unterminated (str, what, startpos) end
    local char = strsub (str, pos, pos)
    if char == closechar then
      return tbl, pos + 1
    end
    local val1, err
    val1, pos, err = scanvalue (str, pos, nullval, objectmeta, arraymeta)
    if err then return nil, pos, err end
    pos = scanwhite (str, pos)
    if not pos then return unterminated (str, what, startpos) end
    char = strsub (str, pos, pos)
    if char == ":" then
      if val1 == nil then
        return nil, pos, "cannot use nil as table index (at " .. loc (str, pos) .. ")"
      end
      pos = scanwhite (str, pos + 1)
      if not pos then return unterminated (str, what, startpos) end
      local val2
      val2, pos, err = scanvalue (str, pos, nullval, objectmeta, arraymeta)
      if err then return nil, pos, err end
      tbl[val1] = val2
      pos = scanwhite (str, pos)
      if not pos then return unterminated (str, what, startpos) end
      char = strsub (str, pos, pos)
    else
      n = n + 1
      tbl[n] = val1
    end
    if char == "," then
      pos = pos + 1
    end
  end
end

scanvalue = function (str, pos, nullval, objectmeta, arraymeta)
  pos = pos or 1
  pos = scanwhite (str, pos)
  if not pos then
    return nil, strlen (str) + 1, "no valid JSON value (reached the end)"
  end
  local char = strsub (str, pos, pos)
  if char == "{" then
    return scantable ('object', "}", str, pos, nullval, objectmeta, arraymeta)
  elseif char == "[" then
    return scantable ('array', "]", str, pos, nullval, objectmeta, arraymeta)
  elseif char == "\"" then
    return scanstring (str, pos)
  else
    local pstart, pend = strfind (str, "^%-?[%d%.]+[eE]?[%+%-]?%d*", pos)
    if pstart then
      local number = str2num (strsub (str, pstart, pend))
      if number then
        return number, pend + 1
      end
    end
    pstart, pend = strfind (str, "^%a%w*", pos)
    if pstart then
      local name = strsub (str, pstart, pend)
      if name == "true" then
        return true, pend + 1
      elseif name == "false" then
        return false, pend + 1
      elseif name == "null" then
        return nullval, pend + 1
      end
    end
    return nil, pos, "no valid JSON value at " .. loc (str, pos)
  end
end

local function optionalmetatables(...)
  if select("#", ...) > 0 then
    return ...
  else
    return {__jsontype = 'object'}, {__jsontype = 'array'}
  end
end

function json.decode (str, pos, nullval, ...)
  local objectmeta, arraymeta = optionalmetatables(...)
  return scanvalue (str, pos, nullval, objectmeta, arraymeta)
end

function json.use_lpeg ()
  local g = require ("lpeg")

  if type(g.version) == 'function' and g.version() == "0.11" then
    error "due to a bug in LPeg 0.11, it cannot be used for JSON matching"
  end

  local pegmatch = g.match
  local P, S, R = g.P, g.S, g.R

  local function ErrorCall (str, pos, msg, state)
    if not state.msg then
      state.msg = msg .. " at " .. loc (str, pos)
      state.pos = pos
    end
    return false
  end

  local function Err (msg)
    return g.Cmt (g.Cc (msg) * g.Carg (2), ErrorCall)
  end

  local function ErrorUnterminatedCall (str, pos, what, state)
    return ErrorCall (str, pos - 1, "unterminated " .. what, state)
  end

  local SingleLineComment = P"//" * (1 - S"\n\r")^0
  local MultiLineComment = P"/*" * (1 - P"*/")^0 * P"*/"
  local Space = (S" \n\r\t" + P"\239\187\191" + SingleLineComment + MultiLineComment)^0

  local function ErrUnterminated (what)
    return g.Cmt (g.Cc (what) * g.Carg (2), ErrorUnterminatedCall)
  end

  local PlainChar = 1 - S"\"\\\n\r"
  local EscapeSequence = (P"\\" * g.C (S"\"\\/bfnrt" + Err "unsupported escape sequence")) / escapechars
  local HexDigit = R("09", "af", "AF")
  local function UTF16Surrogate (match, pos, high, low)
    high, low = tonumber (high, 16), tonumber (low, 16)
    if 0xD800 <= high and high <= 0xDBff and 0xDC00 <= low and low <= 0xDFFF then
      return true, unichar ((high - 0xD800)  * 0x400 + (low - 0xDC00) + 0x10000)
    else
      return false
    end
  end
  local function UTF16BMP (hex)
    return unichar (tonumber (hex, 16))
  end
  local U16Sequence = (P"\\u" * g.C (HexDigit * HexDigit * HexDigit * HexDigit))
  local UnicodeEscape = g.Cmt (U16Sequence * U16Sequence, UTF16Surrogate) + U16Sequence/UTF16BMP
  local Char = UnicodeEscape + EscapeSequence + PlainChar
  local String = P"\"" * (g.Cs (Char ^ 0) * P"\"" + ErrUnterminated "string")
  local Integer = P"-"^(-1) * (P"0" + (R"19" * R"09"^0))
  local Fractal = P"." * R"09"^0
  local Exponent = (S"eE") * (S"+-")^(-1) * R"09"^1
  local Number = (Integer * Fractal^(-1) * Exponent^(-1))/str2num
  local Constant = P"true" * g.Cc (true) + P"false" * g.Cc (false) + P"null" * g.Carg (1)
  local SimpleValue = Number + String + Constant
  local ArrayContent, ObjectContent

  -- The functions parsearray and parseobject parse only a single value/pair
  -- at a time and store them directly to avoid hitting the LPeg limits.
  local function parsearray (str, pos, nullval, state)
    local obj, cont
    local start = pos
    local npos
    local t, nt = {}, 0
    repeat
      obj, cont, npos = pegmatch (ArrayContent, str, pos, nullval, state)
      if cont == 'end' then
        return ErrorUnterminatedCall (str, start, "array", state)
      end
      pos = npos
      if cont == 'cont' or cont == 'last' then
        nt = nt + 1
        t[nt] = obj
      end
    until cont ~= 'cont'
    return pos, setmetatable (t, state.arraymeta)
  end

  local function parseobject (str, pos, nullval, state)
    local obj, key, cont
    local start = pos
    local npos
    local t = {}
    repeat
      key, obj, cont, npos = pegmatch (ObjectContent, str, pos, nullval, state)
      if cont == 'end' then
        return ErrorUnterminatedCall (str, start, "object", state)
      end
      pos = npos
      if cont == 'cont' or cont == 'last' then
        t[key] = obj
      end
    until cont ~= 'cont'
    return pos, setmetatable (t, state.objectmeta)
  end

  local Array = P"[" * g.Cmt (g.Carg(1) * g.Carg(2), parsearray)
  local Object = P"{" * g.Cmt (g.Carg(1) * g.Carg(2), parseobject)
  local Value = Space * (Array + Object + SimpleValue)
  local ExpectedValue = Value + Space * Err "value expected"
  local ExpectedKey = String + Err "key expected"
  local End = P(-1) * g.Cc'end'
  local ErrInvalid = Err "invalid JSON"
  ArrayContent = (Value * Space * (P"," * g.Cc'cont' + P"]" * g.Cc'last'+ End + ErrInvalid)  + g.Cc(nil) * (P"]" * g.Cc'empty' + End  + ErrInvalid)) * g.Cp()
  local Pair = g.Cg (Space * ExpectedKey * Space * (P":" + Err "colon expected") * ExpectedValue)
  ObjectContent = (g.Cc(nil) * g.Cc(nil) * P"}" * g.Cc'empty' + End + (Pair * Space * (P"," * g.Cc'cont' + P"}" * g.Cc'last' + End + ErrInvalid) + ErrInvalid)) * g.Cp()
  local DecodeValue = ExpectedValue * g.Cp ()

  jsonlpeg.version = json.version
  jsonlpeg.encode = json.encode
  jsonlpeg.null = json.null
  jsonlpeg.quotestring = json.quotestring
  jsonlpeg.addnewline = json.addnewline
  jsonlpeg.encodeexception = json.encodeexception
  jsonlpeg.using_lpeg = true

  function jsonlpeg.decode (str, pos, nullval, ...)
    local state = {}
    state.objectmeta, state.arraymeta = optionalmetatables(...)
    local obj, retpos = pegmatch (DecodeValue, str, pos, nullval, state)
    if state.msg then
      return nil, state.pos, state.msg
    else
      return obj, retpos
    end
  end

  -- cache result of this function:
  json.use_lpeg = function () return jsonlpeg end
  jsonlpeg.use_lpeg = json.use_lpeg

  return jsonlpeg
end

if always_use_lpeg then
  return json.use_lpeg()
end

return json
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.audio" ] = function( ... ) local arg = _G.arg;
local bit32 = require("vendor.bitop-lua").bit32

local Audio = {}

function Audio.new(modules)
    local io = modules.io
    local timers = modules.timers
    local ports = io.ports

    local audio = {}

    -- Note: for simplicity, we sample at 44100 Hz. Deal. I'll not bother
    -- to implement any other sampling frequencies until this is more stable.

    audio.buffer = {}
    audio.tone1 = {}
    audio.tone2 = {}
    audio.wave3 = {}
    audio.noise4 = {}

    local next_sample = 0
    local next_sample_cycle = 0

    audio.reset = function()
        audio.tone1.debug_disabled = false
        audio.tone1.period = 128 -- in cycles
        audio.tone1.volume_initial = 0
        audio.tone1.volume_direction = 1
        audio.tone1.volume_step_length = 0 -- in cycles
        audio.tone1.max_length = 0     -- in cycles
        audio.tone1.continuous = false
        audio.tone1.duty_length = .75  -- percentage, from 0-1
        audio.tone1.wave_pattern = 0
        audio.tone1.base_cycle = 0
        audio.tone1.frequency_last_update = 0 -- in cycles
        audio.tone1.wave_duty_counter = 0
        audio.tone1.period_counter = 0
        audio.tone1.frequency_target = 0
        audio.tone1.frequency_shadow = 0
        audio.tone1.frequency_shift_time = 0 -- in cycles, 0 == disabled
        audio.tone1.frequency_shift_counter = 0 -- should be reset on trigger
        audio.tone1.frequency_shift_direction = 1
        audio.tone1.frequency_shift_amount = 0
        audio.tone1.active = false

        audio.tone2.debug_disabled = false
        audio.tone2.period = 128 -- in cycles
        audio.tone2.volume_initial = 0
        audio.tone2.volume_direction = 1
        audio.tone2.volume_step_length = 0 -- in cycles
        audio.tone2.max_length = 0     -- in cycles
        audio.tone2.continuous = false
        audio.tone2.duty_length = .75  -- percentage, from 0-1
        audio.tone2.wave_pattern = 0
        audio.tone2.base_cycle = 0
        audio.tone2.frequency_last_update = 0 -- in cycles
        audio.tone2.period_counter = 0
        audio.tone2.wave_duty_counter = 0
        audio.tone2.frequency_shadow = 0
        audio.tone2.active = false

        audio.wave3.debug_disabled = false
        audio.wave3.enabled = false
        audio.wave3.max_length = 0 -- in cycles
        audio.wave3.volume_shift = 0
        audio.wave3.period = 0 -- in cycles
        audio.wave3.continuous = false
        audio.wave3.base_cycle = 0
        audio.wave3.frequency_last_update = 0 -- in cycles
        audio.wave3.period_counter = 0
        audio.wave3.sample_index = 0
        audio.wave3.frequency_shadow = 0
        audio.wave3.active = false

        audio.noise4.debug_disabled = false
        audio.noise4.volume_initial = 0
        audio.noise4.volume_direction = 1
        audio.noise4.volume_step_length = 0 -- in cycles
        audio.noise4.max_length = 0     -- in cycles
        audio.noise4.continuous = false
        audio.noise4.base_cycle = 0
        audio.noise4.polynomial_period = 16
        audio.noise4.polynomial_lfsr = 0x7FFF -- 15 bits
        audio.noise4.polynomial_last_shift = 0 -- in cycles
        audio.noise4.polynomial_wide = true
        audio.noise4.active = false

        next_sample = 0
        next_sample_cycle = 0

        -- initialize audio registers
        -- pulled from: http://bgb.bircd.org/pandocs.htm#powerupsequence
        io.ram[0x10] = 0x80
        io.ram[0x11] = 0xBF
        io.ram[0x12] = 0xF3
        io.ram[0x14] = 0xBF
        io.ram[0x16] = 0x3F
        io.ram[0x17] = 0x00
        io.ram[0x19] = 0xBF
        io.ram[0x1A] = 0x7F
        io.ram[0x1B] = 0xFF
        io.ram[0x1C] = 0x9F
        io.ram[0x1E] = 0xBF
        io.ram[0x20] = 0xFF
        io.ram[0x21] = 0x00
        io.ram[0x22] = 0x00
        io.ram[0x23] = 0xBF
        io.ram[0x24] = 0x77
        io.ram[0x25] = 0xF3
        io.ram[0x26] = 0xF1
    end

    audio.initialize = function()
        for i = 0, 32768 do
            audio.buffer[i] = 0
        end

        audio.reset()
    end

    audio.save_state = function()
        local state = {}
        state.next_sample_cycle = next_sample_cycle
        return state
    end

    audio.load_state = function(state)
        next_sample_cycle = state.next_sample_cycle
    end

    local wave_patterns = {}
    wave_patterns[0] = .125
    wave_patterns[1] = .25
    wave_patterns[2] = .50
    wave_patterns[3] = .75

    local wave_pattern_tables = {}
    wave_pattern_tables[0] = { 0, 0, 0, 0, 0, 0, 0, 1 }
    wave_pattern_tables[1] = { 1, 0, 0, 0, 0, 0, 0, 1 }
    wave_pattern_tables[2] = { 1, 0, 0, 0, 0, 1, 1, 1 }
    wave_pattern_tables[3] = { 0, 1, 1, 1, 1, 1, 1, 0 }

    io.read_logic[0x26] = function()
        local high_nybble = bit32.band(0xF0, io.ram[0x26])
        local low_nybble = 0
        if audio.tone1.active then
            low_nybble = low_nybble + 0x01
        end
        if audio.tone2.active then
            low_nybble = low_nybble + 0x02
        end
        if audio.wave3.active then
            low_nybble = low_nybble + 0x04
        end
        if audio.noise4.active then
            low_nybble = low_nybble + 0x08
        end
        return high_nybble + low_nybble
    end

    io.write_logic[ports.NR10] = function(byte)
        audio.generate_pending_samples()
        io.ram[ports.NR10] = byte
        local sweep_time = bit32.rshift(bit32.band(byte, 0x70), 4)
        audio.tone1.frequency_shift_time = sweep_time * 32768
        if bit32.band(byte, 0x08) ~= 0 then
            audio.tone1.frequency_shift_direction = -1
        else
            audio.tone1.frequency_shift_direction = 1
        end
        audio.tone1.frequency_shift_amount = bit32.band(byte, 0x07)
    end

    -- Channel 1 Sound Length / Wave Pattern Duty
    io.write_logic[ports.NR11] = function(byte)
        audio.generate_pending_samples()
        io.ram[ports.NR11] = byte
        local wave_pattern = bit32.rshift(bit32.band(byte, 0xC0), 6)
        audio.tone1.duty_length = wave_patterns[wave_pattern]
        audio.tone1.wave_pattern = wave_pattern
        local length_data = bit32.band(byte, 0x3F)
        local length_cycles = (64 - length_data) * 16384
        audio.tone1.max_length = length_cycles
    end

    -- Channel 1 Volume Envelope
    io.write_logic[ports.NR12] = function(byte)
        audio.generate_pending_samples()
        io.ram[ports.NR12] = byte
        audio.tone1.volume_initial = bit32.rshift(bit32.band(byte, 0xF0), 4)
        local direction = bit32.band(byte, 0x08)
        if direction > 0 then
            audio.tone1.volume_direction = 1
        else
            audio.tone1.volume_direction = -1
        end
        local envelope_step_data = bit32.band(byte, 0x07)
        local envelope_step_cycles = envelope_step_data * 65536
        audio.tone1.volume_step_length = envelope_step_cycles
    end

    -- Channel 1 Frequency - Low Bits
    io.write_logic[ports.NR13] = function(byte)
        audio.generate_pending_samples()
        io.ram[ports.NR13] = byte
        local freq_high = bit32.lshift(bit32.band(io.ram[ports.NR14], 0x07), 8)
        local freq_low = byte
        local freq_value = freq_high + freq_low
        audio.tone1.period = 32 * (2048 - freq_value)
        audio.tone1.frequency_shadow = freq_value
    end

    -- Channel 1 Frequency and Trigger - High Bits
    io.write_logic[ports.NR14] = function(byte)
        audio.generate_pending_samples()
        io.ram[ports.NR14] = byte
        local restart = (bit32.band(byte, 0x80) ~= 0)
        local continuous = (bit32.band(byte, 0x40) == 0)
        local freq_high = bit32.lshift(bit32.band(byte, 0x07), 8)
        local freq_low = io.ram[ports.NR13]
        local freq_value = freq_high + freq_low

        audio.tone1.period = 32 * (2048 - freq_value)
        audio.tone1.continuous = continuous
        if restart then
            audio.tone1.base_cycle = timers.system_clock
            audio.tone1.active = true
        end
        audio.tone1.frequency_shadow = freq_value
        audio.tone1.period_conter = (2048 - freq_value)
        audio.tone1.frequency_shift_counter = 0
    end

    -- Channel 2 Sound Length / Wave Pattern Duty
    io.write_logic[ports.NR21] = function(byte)
        audio.generate_pending_samples()
        io.ram[ports.NR21] = byte
        local wave_pattern = bit32.rshift(bit32.band(byte, 0xC0), 6)
        audio.tone2.duty_length = wave_patterns[wave_pattern]
        audio.tone2.wave_pattern = wave_pattern
        local length_data = bit32.band(byte, 0x3F)
        local length_cycles = (64 - length_data) * 16384
        audio.tone2.max_length = length_cycles
    end

    -- Channel 2 Volume Envelope
    io.write_logic[ports.NR22] = function(byte)
        audio.generate_pending_samples()
        io.ram[ports.NR22] = byte
        audio.tone2.volume_initial = bit32.rshift(bit32.band(byte, 0xF0), 4)
        local direction = bit32.band(byte, 0x08)
        if direction > 0 then
            audio.tone2.volume_direction = 1
        else
            audio.tone2.volume_direction = -1
        end
        local envelope_step_data = bit32.band(byte, 0x07)
        local envelope_step_cycles = envelope_step_data * 65536
        audio.tone2.volume_step_length = envelope_step_cycles
    end

    -- Channel 2 Frequency - Low Bits
    io.write_logic[ports.NR23] = function(byte)
        audio.generate_pending_samples()
        io.ram[ports.NR23] = byte
        local freq_high = bit32.lshift(bit32.band(io.ram[ports.NR24], 0x07), 8)
        local freq_low = byte
        local freq_value = freq_high + freq_low
        audio.tone2.period = 32 * (2048 - freq_value)
        audio.tone2.frequency_shadow = freq_value
    end

    -- Channel 2 Frequency and Trigger - High Bits
    io.write_logic[ports.NR24] = function(byte)
        audio.generate_pending_samples()
        io.ram[ports.NR24] = byte
        local restart = (bit32.band(byte, 0x80) ~= 0)
        local continuous = (bit32.band(byte, 0x40) == 0)
        local freq_high = bit32.lshift(bit32.band(byte, 0x07), 8)
        local freq_low = io.ram[ports.NR23]
        local freq_value = freq_high + freq_low

        audio.tone2.period = 32 * (2048 - freq_value)
        audio.tone2.period_conter = (2048 - freq_value)
        audio.tone2.frequency_shadow = freq_value
        audio.tone2.continuous = continuous
        if restart then
            audio.tone2.base_cycle = timers.system_clock
            audio.tone2.active = true
        end
    end

    -- Channel 3 Enabled
    io.write_logic[ports.NR30] = function(byte)
        audio.generate_pending_samples()
        io.ram[ports.NR30] = byte
        audio.wave3.enabled = bit32.band(byte, 0x80) ~= 0
    end

    -- Channel 3 Length
    io.write_logic[ports.NR31] = function(byte)
        audio.generate_pending_samples()
        io.ram[ports.NR31] = byte
        local length_cycles = (256 - byte) * 4096
        audio.wave3.max_length = length_cycles
    end

    -- Channel 3 Volume
    local volume_shift_mappings = {}
    volume_shift_mappings[0] = 4
    volume_shift_mappings[1] = 0
    volume_shift_mappings[2] = 1
    volume_shift_mappings[3] = 2
    io.write_logic[ports.NR32] = function(byte)
        audio.generate_pending_samples()
        io.ram[ports.NR32] = byte
        local volume_select = bit32.rshift(bit32.band(byte, 0x60), 5)
        audio.wave3.volume_shift = volume_shift_mappings[volume_select]
    end

    -- Channel 3 Frequency - Low Bits
    io.write_logic[ports.NR33] = function(byte)
        audio.generate_pending_samples()
        io.ram[ports.NR33] = byte
        local freq_high = bit32.lshift(bit32.band(io.ram[ports.NR34], 0x07), 8)
        local freq_low = byte
        local freq_value = freq_high + freq_low
        audio.wave3.period = 64 * (2048 - freq_value)
        audio.wave3.frequency_shadow = freq_value
    end

    -- Channel 3 Frequency and Trigger - High Bits
    io.write_logic[ports.NR34] = function(byte)
        audio.generate_pending_samples()
        io.ram[ports.NR34] = byte
        local restart = (bit32.band(byte, 0x80) ~= 0)
        local continuous = (bit32.band(byte, 0x40) == 0)
        local freq_high = bit32.lshift(bit32.band(byte, 0x07), 8)
        local freq_low = io.ram[ports.NR33]
        local freq_value = freq_high + freq_low

        audio.wave3.period = 64 * (2048 - freq_value)
        audio.wave3.period_conter = (2048 - freq_value)
        audio.wave3.frequency_shadow = freq_value
        audio.wave3.continuous = continuous
        if restart then
            audio.wave3.base_cycle = timers.system_clock
            audio.wave3.sample_index = 0
            audio.wave3.active = true
        end
    end

    -- Channel 4 Length
    io.write_logic[ports.NR41] = function(byte)
        audio.generate_pending_samples()
        io.ram[ports.NR41] = byte
        local wave_pattern = bit32.rshift(bit32.band(byte, 0xC0), 6)
        audio.noise4.duty_length = wave_patterns[wave_pattern]
        local length_data = bit32.band(byte, 0x3F)
        local length_cycles = (64 - length_data) * 16384
        audio.noise4.max_length = length_cycles
    end

    -- Channel 4 Volume Envelope
    io.write_logic[ports.NR42] = function(byte)
        audio.generate_pending_samples()
        io.ram[ports.NR42] = byte
        audio.noise4.volume_initial = bit32.rshift(bit32.band(byte, 0xF0), 4)
        local direction = bit32.band(byte, 0x08)
        if direction > 0 then
            audio.noise4.volume_direction = 1
        else
            audio.noise4.volume_direction = -1
        end
        local envelope_step_data = bit32.band(byte, 0x07)
        local envelope_step_cycles = envelope_step_data * 65536
        audio.noise4.volume_step_length = envelope_step_cycles
    end

    local polynomial_divisors = {}
    polynomial_divisors[0] = 8
    polynomial_divisors[1] = 16
    polynomial_divisors[2] = 32
    polynomial_divisors[3] = 48
    polynomial_divisors[4] = 64
    polynomial_divisors[5] = 80
    polynomial_divisors[6] = 96
    polynomial_divisors[7] = 112

    -- Channel 4 Polynomial Counter
    io.write_logic[ports.NR43] = function(byte)
        audio.generate_pending_samples()
        io.ram[ports.NR43] = byte
        local shift_clock_frequency = bit32.rshift(bit32.band(byte, 0xF0), 4)
        local wide_step = bit32.band(byte, 0x08) == 0
        local dividing_ratio = polynomial_divisors[bit32.band(byte, 0x07)]

        -- Maybe?
        audio.noise4.polynomial_period = bit32.lshift(dividing_ratio, shift_clock_frequency)
        audio.noise4.polynomial_wide = wide_step
    end

    -- Channel 4 Trigger
    io.write_logic[ports.NR44] = function(byte)
        audio.generate_pending_samples()
        io.ram[ports.NR44] = byte
        local restart = (bit32.band(byte, 0x80) ~= 0)
        local continuous = (bit32.band(byte, 0x40) == 0)

        audio.noise4.continuous = continuous
        if restart then
            audio.noise4.base_cycle = timers.system_clock
            -- Reset the LSFR to all 1's
            audio.noise4.polynomial_lfsr = 0x7FFF
            audio.noise4.active = true
        end
    end

    audio.tone1.update_frequency_shift = function(clock_cycle)
        local tone1 = audio.tone1
        -- A shift_time of 0 disables frequency shifting entirely
        if tone1.frequency_shift_time > 0 then
            local next_edge = tone1.base_cycle + tone1.frequency_shift_time * tone1.frequency_shift_counter
            if clock_cycle >= next_edge then
                local adjustment = bit32.rshift(tone1.frequency_shadow, tone1.frequency_shift_amount) *
                tone1.frequency_shift_direction
                tone1.frequency_shadow = tone1.frequency_shadow + adjustment
                if tone1.frequency_shadow >= 2048 then
                    tone1.frequency_shadow = 2047
                    tone1.active = false
                end
                tone1.period = 32 * (2048 - tone1.frequency_shadow)
                tone1.frequency_shift_counter = tone1.frequency_shift_counter + 1
            end
        end
    end

    audio.noise4.update_lfsr = function(clock_cycle)
        --print(clock_cycle - audio.noise4.polynomial_last_shift)
        --print(audio.noise4.polynomial_period)
        while clock_cycle - audio.noise4.polynomial_last_shift > audio.noise4.polynomial_period do
            local lfsr = audio.noise4.polynomial_lfsr
            -- Grab the lowest two bits in LSFR and XOR them together
            local bit0 = bit32.band(lfsr, 0x1)
            local bit1 = bit32.rshift(bit32.band(lfsr, 0x2), 1)
            local xor = bit32.bxor(bit0, bit1)
            -- Shift LSFR down by one
            lfsr = bit32.rshift(lfsr, 1)
            -- Place the XOR'd bit into the high bit (14) always
            xor = bit32.lshift(xor, 14)
            lfsr = bit32.bor(xor, lfsr)
            if not audio.noise4.polynomial_wide then
                -- place the XOR'd bit into bit 6 as well
                xor = bit32.rshift(xor, 8)
                lfsr = bit32.bor(xor, bit32.band(lfsr, 0x7FBF))
            end
            audio.noise4.polynomial_last_shift = audio.noise4.polynomial_last_shift + audio.noise4.polynomial_period
            audio.noise4.polynomial_lfsr = lfsr
        end
    end

    audio.tone1.generate_sample = function(clock_cycle)
        audio.tone1.update_frequency_shift(clock_cycle)
        local tone1 = audio.tone1
        local duration = clock_cycle - tone1.base_cycle
        if tone1.continuous or (duration <= tone1.max_length) then
            local volume = tone1.volume_initial
            if tone1.volume_step_length > 0 then
                volume = volume + tone1.volume_direction * math.floor(duration / tone1.volume_step_length)
            end
            if volume > 0 then
                if volume > 0xF then
                    volume = 0xF
                end

                while clock_cycle > tone1.frequency_last_update + 4 do
                    tone1.period_counter = tone1.period_counter - 1
                    if tone1.period_counter <= 0 then
                        tone1.period_counter = (2048 - tone1.frequency_shadow)
                        tone1.wave_duty_counter = tone1.wave_duty_counter + 1
                        if tone1.wave_duty_counter >= 8 then
                            tone1.wave_duty_counter = 0
                        end
                    end
                    tone1.frequency_last_update = tone1.frequency_last_update + 4
                end

                if wave_pattern_tables[tone1.wave_pattern][tone1.wave_duty_counter + 1] == 0 then
                    return volume / 0xF * -1
                else
                    return volume / 0xF
                end
            end
        else
            audio.tone1.active = false
        end
        return 0
    end

    audio.tone2.generate_sample = function(clock_cycle)
        local tone2 = audio.tone2
        local duration = clock_cycle - tone2.base_cycle
        if tone2.continuous or (duration <= tone2.max_length) then
            local volume = tone2.volume_initial
            if tone2.volume_step_length > 0 then
                volume = volume + tone2.volume_direction * math.floor(duration / tone2.volume_step_length)
            end
            if volume > 0 then
                if volume > 0xF then
                    volume = 0xF
                end

                while clock_cycle > tone2.frequency_last_update + 4 do
                    tone2.period_counter = tone2.period_counter - 1
                    if tone2.period_counter <= 0 then
                        tone2.period_counter = (2048 - tone2.frequency_shadow)
                        tone2.wave_duty_counter = tone2.wave_duty_counter + 1
                        if tone2.wave_duty_counter >= 8 then
                            tone2.wave_duty_counter = 0
                        end
                    end
                    tone2.frequency_last_update = tone2.frequency_last_update + 4
                end

                if wave_pattern_tables[tone2.wave_pattern][tone2.wave_duty_counter + 1] == 0 then
                    return volume / 0xF * -1
                else
                    return volume / 0xF
                end
            end
        else
            tone2.active = false
        end
        return 0
    end

    audio.wave3.generate_sample = function(clock_cycle)
        local wave3 = audio.wave3
        local duration = clock_cycle - wave3.base_cycle
        if wave3.enabled then
            if wave3.continuous or (duration <= wave3.max_length) then
                --local period = wave3.period
                --local period_progress = (duration % period) / (period)
                --local sample_index = math.floor(period_progress * 32)
                while clock_cycle > wave3.frequency_last_update + 2 do
                    wave3.period_counter = wave3.period_counter - 1
                    if wave3.period_counter <= 0 then
                        wave3.period_counter = (2048 - wave3.frequency_shadow)
                        wave3.sample_index = wave3.sample_index + 1
                        if wave3.sample_index >= 32 then
                            wave3.sample_index = 0
                        end
                    end
                    wave3.frequency_last_update = wave3.frequency_last_update + 2
                end

                local byte_index = bit32.rshift(wave3.sample_index, 1)
                local sample = io.ram[0x30 + byte_index]
                -- If this is an even numbered sample, shift the high nybble
                -- to the lower nybble
                if wave3.sample_index % 2 == 0 then
                    sample = bit32.rshift(sample, 4)
                end
                -- Regardless, mask out the lower nybble; this becomes our sample to play
                sample = bit32.band(sample, 0x0F)
                -- Shift the sample based on the volume parameter
                sample = bit32.rshift(sample, wave3.volume_shift)
                -- This sample will be from 0-15, we need to adjust it so that it's from -1  to 1
                sample = (sample - 8) / 8
                return sample
            else
                wave3.active = false
            end
        else
            wave3.active = false
        end
        return 0
    end

    audio.noise4.generate_sample = function(clock_cycle)
        audio.noise4.update_lfsr(clock_cycle)
        local noise4 = audio.noise4
        local duration = clock_cycle - noise4.base_cycle
        if noise4.continuous or (duration <= noise4.max_length) then
            local volume = noise4.volume_initial
            if noise4.volume_step_length > 0 then
                volume = volume + noise4.volume_direction * math.floor(duration / noise4.volume_step_length)
            end
            if volume > 0 then
                if volume > 0xF then
                    volume = 0xF
                end
                -- Output high / low is based on the INVERTED low bit of LFSR
                if bit32.band(noise4.polynomial_lfsr, 0x1) == 0 then
                    return volume / 0xF
                else
                    return volume / 0xF * -1
                end
            end
        else
            noise4.active = false
        end
        return 0
    end

    audio.__on_buffer_full = function(buffer) end

    audio.debug = {}
    audio.debug.current_sample = 0
    audio.debug.max_samples = 128
    audio.debug.tone1 = {}
    audio.debug.tone2 = {}
    audio.debug.wave3 = {}
    audio.debug.noise4 = {}
    audio.debug.final = {}
    for i = 0, audio.debug.max_samples do
        audio.debug.tone1[i] = 0
        audio.debug.tone2[i] = 0
        audio.debug.wave3[i] = 0
        audio.debug.noise4[i] = 0
        audio.debug.final[i] = 0
    end

    audio.save_debug_samples = function(tone1, tone2, wave3, noise4, final)
        local debug = audio.debug
        debug.tone1[debug.current_sample] = tone1
        debug.tone2[debug.current_sample] = tone2
        debug.wave3[debug.current_sample] = wave3
        debug.noise4[debug.current_sample] = noise4
        debug.final[debug.current_sample] = final
        debug.current_sample = debug.current_sample + 1
        if debug.current_sample >= debug.max_samples then
            debug.current_sample = 0
        end
    end

    audio.debug.enabled = false

    audio.generate_pending_samples = function()
        while next_sample_cycle < timers.system_clock do
            local tone1            = audio.tone1.generate_sample(next_sample_cycle)
            local tone2            = audio.tone2.generate_sample(next_sample_cycle)
            local wave3            = audio.wave3.generate_sample(next_sample_cycle)
            local noise4           = audio.noise4.generate_sample(next_sample_cycle)

            local sample_left      = 0
            local sample_right     = 0

            local channels_enabled = io.ram[ports.NR51]
            if bit32.band(channels_enabled, 0x80) ~= 0 and not audio.noise4.debug_disabled then
                sample_right = sample_right + noise4
            end
            if bit32.band(channels_enabled, 0x40) ~= 0 and not audio.wave3.debug_disabled then
                sample_right = sample_right + wave3
            end
            if bit32.band(channels_enabled, 0x20) ~= 0 and not audio.tone2.debug_disabled then
                sample_right = sample_right + tone2
            end
            if bit32.band(channels_enabled, 0x10) ~= 0 and not audio.tone1.debug_disabled then
                sample_right = sample_right + tone1
            end

            if bit32.band(channels_enabled, 0x08) ~= 0 and not audio.noise4.debug_disabled then
                sample_left = sample_left + noise4
            end
            if bit32.band(channels_enabled, 0x04) ~= 0 and not audio.wave3.debug_disabled then
                sample_left = sample_left + wave3
            end
            if bit32.band(channels_enabled, 0x02) ~= 0 and not audio.tone2.debug_disabled then
                sample_left = sample_left + tone2
            end
            if bit32.band(channels_enabled, 0x01) ~= 0 and not audio.tone1.debug_disabled then
                sample_left = sample_left + tone1
            end

            sample_right = sample_right / 4
            sample_left = sample_left / 4

            if audio.debug.enabled then
                -- Debug in mono
                audio.save_debug_samples(tone1, tone2, wave3, noise4, (tone1 + tone2 + wave3 + noise4) / 4)
            end

            -- Left/Right Channel Volume
            local right_volume = bit32.rshift(bit32.band(io.ram[ports.NR50], 0x70), 4)
            local left_volume = bit32.band(io.ram[ports.NR50], 0x07)

            sample_right = sample_right * right_volume / 7
            sample_left = sample_left * left_volume / 7

            audio.buffer[next_sample] = sample_left
            next_sample = next_sample + 1
            audio.buffer[next_sample] = sample_right
            next_sample = next_sample + 1
            if next_sample >= 1024 then
                audio.__on_buffer_full(audio.buffer)
                next_sample = 0
            end
            next_sample_cycle = next_sample_cycle + 128 --number of clocks per sample at 32 KHz
        end
    end

    audio.on_buffer_full = function(callback)
        audio.__on_buffer_full = callback
    end

    audio.update = function()
        audio.generate_pending_samples()
    end

    return audio
end

return Audio
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.cartridge" ] = function( ... ) local arg = _G.arg;
local rom_header = require("gameboy.rom_header")

local MbcNone = require("gameboy.mbc.none")
local Mbc1 = require("gameboy.mbc.mbc1")
local Mbc2 = require("gameboy.mbc.mbc2")
local Mbc3 = require("gameboy.mbc.mbc3")
local Mbc5 = require("gameboy.mbc.mbc5")

local Cartridge = {}

function Cartridge.new(modules)
    local cartridge = {}

    local io = modules.io
    local memory = modules.memory
    local ports = io.ports

    local mbc_none = MbcNone.new()
    local mbc1 = Mbc1.new()
    local mbc2 = Mbc2.new()
    local mbc3 = Mbc3.new()
    local mbc5 = Mbc5.new()

    cartridge.external_ram = memory.generate_block(128 * 1024)
    cartridge.external_ram.dirty = false

    local mbc_mappings = {}
    mbc_mappings[0x00] = { mbc = mbc_none, options = {} }
    mbc_mappings[0x01] = { mbc = mbc1, options = {} }
    mbc_mappings[0x02] = { mbc = mbc1, options = {} }
    mbc_mappings[0x03] = { mbc = mbc1, options = {} }

    mbc_mappings[0x05] = { mbc = mbc2, options = {} }
    mbc_mappings[0x06] = { mbc = mbc2, options = {} }

    mbc_mappings[0x0F] = { mbc = mbc3, options = {} }
    mbc_mappings[0x10] = { mbc = mbc3, options = {} }
    mbc_mappings[0x12] = { mbc = mbc3, options = {} }
    mbc_mappings[0x11] = { mbc = mbc3, options = {} }
    mbc_mappings[0x13] = { mbc = mbc3, options = {} }

    mbc_mappings[0x19] = { mbc = mbc5, options = {} }
    mbc_mappings[0x1A] = { mbc = mbc5, options = {} }
    mbc_mappings[0x1B] = { mbc = mbc5, options = {} }
    mbc_mappings[0x1C] = { mbc = mbc5, options = { rumble_pak = true } }
    mbc_mappings[0x1D] = { mbc = mbc5, options = { rumble_pak = true } }
    mbc_mappings[0x1E] = { mbc = mbc5, options = { rumble_pak = true } }

    cartridge.initialize = function(gameboy)
        cartridge.gameboy = gameboy
    end

    cartridge.load = function(file_data, size)
        print("Reading cartridge into memory...")
        cartridge.raw_data = {}
        for i = 0, size - 1 do
            cartridge.raw_data[i] = file_data:byte(i + 1)
        end
        print("Read " .. math.ceil(#cartridge.raw_data / 1024) .. " kB")
        cartridge.header = rom_header.parse_cartridge_header(cartridge.raw_data)
        rom_header.print_cartridge_header(cartridge.header)

        if mbc_mappings[cartridge.header.mbc_type] then
            local MBC = mbc_mappings[cartridge.header.mbc_type].mbc
            for k, v in pairs(mbc_mappings[cartridge.header.mbc_type].options) do
                MBC[k] = v
            end
            print("Using mapper: ", cartridge.header.mbc_name)
            MBC.raw_data = cartridge.raw_data
            MBC.external_ram = cartridge.external_ram
            MBC.header = cartridge.header
            -- Cart ROM
            memory.map_block(0x00, 0x7F, MBC)
            -- External RAM
            memory.map_block(0xA0, 0xBF, MBC, 0x0000)
        else
            local MBC = mbc_mappings[0x00].mbc
            print("Unsupported MBC type! Defaulting to ROM ONLY, game will probably not boot.")
            MBC.raw_data = cartridge.raw_data
            MBC.external_ram = cartridge.external_ram
            memory.map_block(0x00, 0x7F, MBC)
            memory.map_block(0xA0, 0xBF, MBC, 0x0000)
        end

        -- select a gameboy type based on the cart header
        if cartridge.header.color then
            cartridge.gameboy.type = cartridge.gameboy.types.color
        else
            cartridge.gameboy.type = cartridge.gameboy.types.dmg
        end

        -- Add a guard to cartridge.raw_data, such that any out-of-bounds reads return 0x00
        cartridge.raw_data.mt = {}
        cartridge.raw_data.mt.__index = function(table, address)
            -- Data doesn't exist? Tough luck; return 0x00
            return 0x00
        end

        setmetatable(cartridge.raw_data, cartridge.raw_data.mt)
    end

    cartridge.reset = function()
        if cartridge.header then
            -- Simulates a power cycle, resetting selected banks and other variables
            if mbc_mappings[cartridge.header.mbc_type] then
                mbc_mappings[cartridge.header.mbc_type].mbc:reset()
            else
                -- Calling this for logical completeness, but
                -- mbc_mappings[0x00] is actually type none,
                -- whose reset function is a no-op
                mbc_mappings[0x00].mbc:reset()
            end
        end

        -- TODO: Figure out if we care enough to reset
        -- External RAM here, for games which don't have
        -- a BATTERY in their cartridge type
    end

    cartridge.save_state = function()
        -- Note: for NOW, don't worry about the cartridge
        -- header, and assume a cart swap has not happened
        if mbc_mappings[cartridge.header.mbc_type] then
            return mbc_mappings[cartridge.header.mbc_type].mbc:save_state()
        else
            mbc_mappings[0x00].mbc:save_state()
        end
    end

    cartridge.load_state = function(state_data)
        -- Note: for NOW, don't worry about the cartridge
        -- header, and assume a cart swap has not happened
        if mbc_mappings[cartridge.header.mbc_type] then
            return mbc_mappings[cartridge.header.mbc_type].mbc:load_state(state_data)
        else
            mbc_mappings[0x00].mbc:load_state(state_data)
        end
    end

    return cartridge
end

return Cartridge
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.dma" ] = function( ... ) local arg = _G.arg;
local bit32 = require("vendor.bitop-lua").bit32

local Dma = {}

function Dma.new(modules)
    local dma = {}

    local io = modules.io
    local memory = modules.memory
    local timers = modules.timers
    local ports = io.ports

    dma.source = 0
    dma.destination = 0x8000
    dma.current = 0
    dma.length = 0
    dma.hblank = false
    dma.running = false

    dma.do_hblank = function()
        if dma.hblank then
            for i = 0, 0x10 - 1 do
                memory[dma.destination + i] = memory[dma.source + i]
            end
            dma.source = dma.source + 0x10;
            dma.destination = dma.destination + 0x10;
            dma.length = dma.length - 0x10;
            --print(string.format("HBlank Transfer of 0x10 bytes from %04X to %04X", dma.source, dma.destination))
            if dma.length <= 0 then
                dma.hblank = false
                io.ram[0x55] = 0xFF;
                --print("HBlank transfer finished!");
            else
                io.ram[0x55] = (dma.length / 0x10) - 1;
            end
            -- TODO: Implement clock delay for hblank DMA transfers
        end
    end

    io.write_logic[ports.DMA] = function(byte)
        -- DMA Transfer. Copies data from 0x0000 + 0x100 * byte, into OAM data
        local destmap = memory.block_map[0xfe00]
        local sourcemap = memory.block_map[byte * 0x100]
        local source = 0x0000 + 0x100 * byte
        local destination = 0xFE00
        while destination <= 0xFE9F do
            destmap[destination] = sourcemap[source]
            destination = destination + 1
            source = source + 1
        end
        -- TODO: Implement memory access cooldown; real hardware requires
        -- programs to call DMA transfer from High RAM and then wait there
        -- for several clocks while it finishes.
    end

    io.write_logic[0x51] = function(byte)
        -- DMA source HIGH Byte
        dma.source = bit32.lshift(byte, 8) + bit32.band(dma.source, 0xFF)
    end

    io.write_logic[0x52] = function(byte)
        -- DMA source LOW byte (lower 4 bits ignored, forces alignment to 0x10
        dma.source = bit32.band(dma.source, 0xFF00) + bit32.band(byte, 0xF0)
    end

    io.write_logic[0x53] = function(byte)
        -- DMA destination HIGH byte (upper 3 bits ignored, forced to reside within 0x8000 - 0x9FFF)
        dma.destination = 0x8000 + bit32.lshift(bit32.band(byte, 0x1F), 8) + bit32.band(dma.destination, 0xFF)
    end

    io.write_logic[0x54] = function(byte)
        -- DMA destination LOW byte (lower 4 bits ignored, forces alignment to 0x10
        dma.destination = bit32.band(dma.destination, 0xFF00) + bit32.band(byte, 0xF0)
    end

    io.write_logic[0x55] = function(byte)
        --dma.source = bit32.lshift(io.ram[0x51], 8) + bit32.band(io.ram[0x52], 0xF0)
        --dma.destination = bit32.lshift(bit32.band(io.ram[0x53], 0x1F), 8) + bit32.band(io.ram[0x54], 0xF0) + 0x8000
        dma.length = (bit32.band(byte, 0x7F) + 1) * 16
        if bit32.band(byte, 0x80) ~= 0 then
            dma.hblank = true
            io.ram[0x55] = bit32.band(byte, 0x7F)
            --print(string.format("HBlank DMA from 0x%04X to 0x%04X with length 0x%04X", dma.source, dma.destination, dma.length))
            -- is the screen off, or are we in the middle of hblank? If so, copy a block right away
            current_mode = bit32.band(io.ram[ports.STAT], 0x3)
            display_disabled = bit32.band(io.ram[ports.LCDC], 0x80) == 0
            if current_mode == 0 or display_disabled then
                dma.do_hblank()
            end
        else
            if dma.hblank then
                --print("Stopped an HBlank DMA in progress!")
                -- Terminate the hblank DMA in progress. Do NOT start a general purpose DMA on this write.
                dma.hblank = false
                io.ram[0x55] = bit32.bor(io.ram[0x55], 0x80);
                return
            end
            dma.hblank = false
            -- process the DMA now, adjust clock too. (cheat, basically.)
            for i = 0, dma.length - 1 do
                memory[dma.destination + i] = memory[dma.source + i]
            end
            dma.destination = dma.destination + dma.length

            timers.system_clock = timers.system_clock + dma.length / 2
            io.ram[0x55] = 0xFF
            --print(string.format("General Purpose DMA From: %04X -> %04X Length: %04X", dma.source, dma.destination, dma.length))
        end
    end

    return dma
end

return Dma
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.graphics" ] = function( ... ) local arg = _G.arg;
local bit32 = require("vendor.bitop-lua").bit32

local Cache = require("gameboy.graphics.cache")
local Palette = require("gameboy.graphics.palette")
local Registers = require("gameboy.graphics.registers")

local Graphics = {}

function Graphics.new(modules)
    local interrupts = modules.interrupts
    local dma = modules.dma
    local io = modules.io
    local memory = modules.memory
    local timers = modules.timers
    local processor = modules.processor

    local graphics = {}

    graphics.cache = Cache.new(graphics)
    graphics.palette = Palette.new(graphics, modules)
    graphics.registers = Registers.new(graphics, modules, graphics.cache)

    --just for shortening access
    local ports = io.ports

    -- Internal Variables
    graphics.vblank_count = 0
    graphics.last_edge = 0
    graphics.next_edge = 0
    graphics.lcdstat = false

    graphics.game_screen = {}

    graphics.clear_screen = function()
        for y = 0, 143 do
            graphics.game_screen[y] = {}
            for x = 0, 159 do
                graphics.game_screen[y][x] = { 255, 255, 255 }
            end
        end
    end

    graphics.lcd = {}

    -- Initialize VRAM blocks in main memory
    graphics.vram = memory.generate_block(16 * 2 * 1024, 0x8000)
    graphics.vram.bank = 0
    graphics.vram_map = {}
    graphics.vram_map.mt = {}
    graphics.vram_map.mt.__index = function(table, address)
        return graphics.vram[address + (16 * 1024 * graphics.vram.bank)]
    end
    graphics.vram_map.mt.__newindex = function(table, address, value)
        graphics.vram[address + (16 * 1024 * graphics.vram.bank)] = value
        if address >= 0x8000 and address <= 0x97FF then
            graphics.cache.refreshTile(address, graphics.vram.bank)
        end
        if address >= 0x9800 and address <= 0x9BFF then
            local x = address % 32
            local y = math.floor((address - 0x9800) / 32)
            if graphics.vram.bank == 1 then
                graphics.cache.refreshAttributes(graphics.cache.map_0_attr, x, y, address)
            end
            graphics.cache.refreshTileIndex(x, y, 0x9800, graphics.cache.map_0, graphics.cache.map_0_attr)
        end
        if address >= 0x9C00 and address <= 0x9FFF then
            local x = address % 32
            local y = math.floor((address - 0x9C00) / 32)
            if graphics.vram.bank == 1 then
                graphics.cache.refreshAttributes(graphics.cache.map_1_attr, x, y, address)
            end
            graphics.cache.refreshTileIndex(x, y, 0x9C00, graphics.cache.map_1, graphics.cache.map_1_attr)
        end
    end
    setmetatable(graphics.vram_map, graphics.vram_map.mt)
    memory.map_block(0x80, 0x9F, graphics.vram_map, 0)

    graphics.oam_raw = memory.generate_block(0xA0, 0xFE00)
    graphics.oam = {}
    graphics.oam.mt = {}
    graphics.oam.mt.__index = function(table, address)
        if address <= 0xFE9F then
            return graphics.oam_raw[address]
        end
        -- out of range? So sorry, return nothing
        return 0x00
    end
    graphics.oam.mt.__newindex = function(table, address, byte)
        if address <= 0xFE9F then
            graphics.oam_raw[address] = byte
            graphics.cache.refreshOamEntry(math.floor((address - 0xFE00) / 4))
        end
        -- out of range? So sorry, discard the write
        return
    end
    setmetatable(graphics.oam, graphics.oam.mt)
    memory.map_block(0xFE, 0xFE, graphics.oam, 0)

    io.write_logic[0x4F] = function(byte)
        if graphics.gameboy.type == graphics.gameboy.types.color then
            io.ram[0x4F] = bit32.band(0x1, byte)
            graphics.vram.bank = bit32.band(0x1, byte)
        else
            -- Not sure if the write mask should apply in DMG / SGB mode
            io.ram[0x4F] = byte
        end
    end

    graphics.initialize = function(gameboy)
        graphics.gameboy = gameboy
        graphics.registers.status.SetMode(2)
        graphics.clear_screen()
        graphics.reset()
    end

    graphics.reset = function()
        graphics.cache.reset()
        graphics.palette.reset()

        -- zero out all of VRAM:
        for i = 0x8000, (0x8000 + (16 * 2 * 1024) - 1) do
            graphics.vram[i] = 0
        end

        -- zero out all of OAM
        for i = 0xFE00, 0xFE9F do
            graphics.oam[i] = 0
        end

        graphics.vblank_count = 0
        graphics.last_edge = 0
        graphics.vram.bank = 0
        graphics.lcdstat = false

        graphics.clear_screen()
        graphics.registers.status.SetMode(2)
    end

    graphics.save_state = function()
        local state = {}

        state.vram = {}
        for i = 0x8000, (0x8000 + (16 * 2 * 1024) - 1) do
            state.vram[i] = graphics.vram[i]
        end

        state.vram_bank = graphics.vram.bank

        state.oam = {}
        for i = 0xFE00, 0xFE9F do
            state.oam[i] = graphics.oam[i]
        end

        state.vblank_count  = graphics.vblank_count
        state.last_edge     = graphics.last_edge
        state.lcdstat       = graphics.lcdstat
        state.mode          = graphics.registers.status.mode

        state.palette       = {}
        state.palette.bg    = graphics.palette.bg
        state.palette.obj0  = graphics.palette.obj0
        state.palette.obj1  = graphics.palette.obj1

        state.color_bg      = {}
        state.color_obj     = {}
        state.color_bg_raw  = {}
        state.color_obj_raw = {}

        for p = 0, 7 do
            state.color_bg[p] = graphics.palette.color_bg[p]
            state.color_obj[p] = graphics.palette.color_obj[p]
        end

        for i = 0, 63 do
            state.color_bg_raw[i] = graphics.palette.color_bg_raw[i]
            state.color_obj_raw[i] = graphics.palette.color_obj_raw[i]
        end

        return state
    end

    graphics.load_state = function(state)
        for i = 0x8000, (0x8000 + (16 * 2 * 1024) - 1) do
            graphics.vram[i] = state.vram[i]
        end

        graphics.vram.bank = state.vram_bank

        for i = 0xFE00, 0xFE9F do
            graphics.oam[i] = state.oam[i]
        end
        graphics.vblank_count          = state.vblank_count
        graphics.last_edge             = state.last_edge
        graphics.lcdstat               = state.lcdstat
        graphics.registers.status.mode = state.mode

        graphics.palette.bg            = state.palette.bg
        graphics.palette.obj0          = state.palette.obj0
        graphics.palette.obj1          = state.palette.obj1

        for p = 0, 7 do
            graphics.palette.color_bg[p] = state.color_bg[p]
            graphics.palette.color_obj[p] = state.color_obj[p]
        end

        for i = 0, 63 do
            graphics.palette.color_bg_raw[i] = state.color_bg_raw[i]
            graphics.palette.color_obj_raw[i] = state.color_obj_raw[i]
        end

        graphics.cache.refreshAll()
        io.write_logic[ports.STAT](io.ram[ports.STAT])
        io.write_logic[ports.LCDC](io.ram[ports.LCDC])
    end

    local time_at_this_mode = function()
        return timers.system_clock - graphics.last_edge
    end

    local scanline_data = {}
    scanline_data.x = 0
    scanline_data.bg_tile_x = 0
    scanline_data.bg_tile_y = 0
    scanline_data.sub_x = 0
    scanline_data.sub_y = 0
    scanline_data.active_tile = nil
    scanline_data.active_attr = nil
    scanline_data.current_map = nil
    scanline_data.current_map_attr = nil
    scanline_data.window_active = false
    scanline_data.bg_index = {}
    scanline_data.bg_priority = {}
    scanline_data.active_palette = nil

    graphics.refresh_lcdstat = function()
        local lcdstat = false
        local status = graphics.registers.status

        lcdstat =
            (status.lyc_interrupt_enabled and io.ram[ports.LY] == io.ram[ports.LYC]) or
            (status.oam_interrupt_enabled and status.mode == 2) or
            (status.vblank_interrupt_enabled and status.mode == 1) or
            (status.hblank_interrupt_enabled and status.mode == 0)

        -- If this is a *rising* edge, raise the LCDStat interrupt
        if graphics.lcdstat == false and lcdstat == true then
            interrupts.raise(interrupts.LCDStat)
        end

        graphics.lcdstat = lcdstat
    end

    io.write_logic[ports.LY] = function(byte)
        -- LY, writes reset the counter
        io.ram[ports.LY] = 0
        graphics.refresh_lcdstat()
    end

    io.write_logic[ports.LYC] = function(byte)
        -- LY, writes reset the counter
        io.ram[ports.LYC] = byte
        graphics.refresh_lcdstat()
    end

    -- HBlank: Period between scanlines
    local handle_mode = {}
    handle_mode[0] = function()
        if timers.system_clock - graphics.last_edge > 204 then
            graphics.last_edge = graphics.last_edge + 204
            io.ram[ports.LY] = io.ram[ports.LY] + 1
            if io.ram[ports.LY] == io.ram[ports.LYC] then
                -- set the LY compare bit
                io.ram[ports.STAT] = bit32.bor(io.ram[ports.STAT], 0x4)
            else
                -- clear the LY compare bit
                io.ram[ports.STAT] = bit32.band(io.ram[ports.STAT], 0xFB)
            end

            if io.ram[ports.LY] >= 144 then
                graphics.registers.status.SetMode(1)
                graphics.vblank_count = graphics.vblank_count + 1
                interrupts.raise(interrupts.VBlank)
            else
                graphics.registers.status.SetMode(2)
            end

            graphics.refresh_lcdstat()
        else
            graphics.next_edge = graphics.last_edge + 204
        end
    end

    --VBlank: nothing to do except wait for the next frame
    handle_mode[1] = function()
        if timers.system_clock - graphics.last_edge > 456 then
            graphics.last_edge = graphics.last_edge + 456
            io.ram[ports.LY] = io.ram[ports.LY] + 1
            graphics.refresh_lcdstat()
        else
            graphics.next_edge = graphics.last_edge + 456
        end

        if io.ram[ports.LY] >= 154 then
            io.ram[ports.LY] = 0
            graphics.initialize_frame()
            graphics.registers.status.SetMode(2)
            graphics.refresh_lcdstat()
        end

        if io.ram[ports.LY] == io.ram[ports.LYC] then
            -- set the LY compare bit
            io.ram[ports.STAT] = bit32.bor(io.ram[ports.STAT], 0x4)
        else
            -- clear the LY compare bit
            io.ram[ports.STAT] = bit32.band(io.ram[ports.STAT], 0xFB)
        end
    end

    -- OAM Read: OAM cannot be accessed
    handle_mode[2] = function()
        if timers.system_clock - graphics.last_edge > 80 then
            graphics.last_edge = graphics.last_edge + 80
            graphics.initialize_scanline()
            graphics.registers.status.SetMode(3)
            graphics.refresh_lcdstat()
        else
            graphics.next_edge = graphics.last_edge + 80
        end
    end
    -- VRAM Read: Neither VRAM, OAM, nor CGB palettes can be read
    handle_mode[3] = function()
        local duration = timers.system_clock - graphics.last_edge
        graphics.draw_next_pixels(duration)
        if timers.system_clock - graphics.last_edge > 172 then
            graphics.last_edge = graphics.last_edge + 172
            graphics.draw_sprites_into_scanline(io.ram[ports.LY], scanline_data.bg_index, scanline_data.bg_priority)
            graphics.registers.status.SetMode(0)
            -- If enabled, fire an HBlank interrupt
            graphics.refresh_lcdstat()
            -- If the hblank dma is active, copy the next block
            dma.do_hblank()
        else
            graphics.next_edge = graphics.last_edge + 172
        end
    end

    graphics.update = function()
        if graphics.registers.display_enabled then
            handle_mode[graphics.registers.status.mode]()
        else
            -- erase our clock debt, so we don't do stupid timing things when the
            -- display is enabled again later
            graphics.last_edge = timers.system_clock
            graphics.next_edge = timers.system_clock
            graphics.registers.status.SetMode(0)
            io.ram[ports.LY] = 0
            graphics.refresh_lcdstat()
        end
    end

    local function plot_pixel(buffer, x, y, r, g, b)
        buffer[y][x][1] = r
        buffer[y][x][2] = g
        buffer[y][x][3] = b
    end

    local frame_data = {}
    frame_data.window_pos_y = 0
    frame_data.window_draw_y = 0

    graphics.initialize_frame = function()
        -- latch WY at the beginning of the *frame*
        frame_data.window_pos_y = io.ram[ports.WY]
        frame_data.window_draw_y = 0
    end

    graphics.initialize_scanline = function()
        scanline_data.x = 0

        scanline_data.bg_tile_x = math.floor(io.ram[ports.SCX] / 8)
        scanline_data.bg_tile_y = math.floor((io.ram[ports.LY] + io.ram[ports.SCY]) / 8)
        if scanline_data.bg_tile_y >= 32 then
            scanline_data.bg_tile_y = scanline_data.bg_tile_y - 32
        end

        scanline_data.sub_x = io.ram[ports.SCX] % 8
        scanline_data.sub_y = (io.ram[ports.LY] + io.ram[ports.SCY]) % 8

        scanline_data.current_map = graphics.registers.background_tilemap
        scanline_data.current_map_attr = graphics.registers.background_attr

        scanline_data.active_attr = scanline_data.current_map_attr[scanline_data.bg_tile_x][scanline_data.bg_tile_y]
        scanline_data.active_tile = scanline_data.current_map[scanline_data.bg_tile_x][scanline_data.bg_tile_y]
        scanline_data.window_active = false
    end

    graphics.switch_to_window = function()
        local ly = io.ram[ports.LY]
        local w_x = io.ram[ports.WX] - 7
        if graphics.registers.window_enabled and scanline_data.x >= w_x and ly >= frame_data.window_pos_y then
            -- switch to window map
            scanline_data.current_map = graphics.registers.window_tilemap
            scanline_data.current_map_attr = graphics.registers.window_attr
            scanline_data.bg_tile_x = math.floor((scanline_data.x - w_x) / 8)
            scanline_data.bg_tile_y = math.floor(frame_data.window_draw_y / 8)
            scanline_data.sub_x = (scanline_data.x - w_x) % 8
            scanline_data.sub_y = (frame_data.window_draw_y) % 8
            frame_data.window_draw_y = frame_data.window_draw_y + 1
            if frame_data.window_draw_y > 143 then
                frame_data.window_draw_y = 143
            end

            scanline_data.active_attr = scanline_data.current_map_attr[scanline_data.bg_tile_x][scanline_data.bg_tile_y]
            scanline_data.active_tile = scanline_data.current_map[scanline_data.bg_tile_x][scanline_data.bg_tile_y]
            scanline_data.window_active = true
        end
    end

    graphics.draw_next_pixels = function(duration)
        local ly = io.ram[ports.LY]
        local game_screen = graphics.game_screen

        while scanline_data.x < duration and scanline_data.x < 160 do
            local dx = scanline_data.x
            if not scanline_data.window_active then
                graphics.switch_to_window()
            end

            local bg_index = 0 --default, in case no background is enabled
            if graphics.registers.background_enabled then
                -- DRAW BG PIXEL HERE
                local sub_x = scanline_data.sub_x
                local sub_y = scanline_data.sub_y
                bg_index = scanline_data.active_tile[sub_x][sub_y]
                local active_palette = scanline_data.active_attr.palette[bg_index]

                game_screen[ly][dx][1] = active_palette[1]
                game_screen[ly][dx][2] = active_palette[2]
                game_screen[ly][dx][3] = active_palette[3]
            end

            scanline_data.bg_index[scanline_data.x] = bg_index
            scanline_data.bg_priority[scanline_data.x] = scanline_data.active_attr.priority

            scanline_data.x = scanline_data.x + 1
            scanline_data.sub_x = scanline_data.sub_x + 1
            if scanline_data.sub_x > 7 then
                -- fetch next tile
                scanline_data.sub_x = 0
                scanline_data.bg_tile_x = scanline_data.bg_tile_x + 1
                if scanline_data.bg_tile_x >= 32 then
                    scanline_data.bg_tile_x = scanline_data.bg_tile_x - 32
                end
                if not scanline_data.window_active then
                    scanline_data.sub_y = (ly + io.ram[ports.SCY]) % 8
                    scanline_data.bg_tile_y = math.floor((ly + io.ram[ports.SCY]) / 8)
                    if scanline_data.bg_tile_y >= 32 then
                        scanline_data.bg_tile_y = scanline_data.bg_tile_y - 32
                    end
                end

                local tile_attr = scanline_data.current_map_attr[scanline_data.bg_tile_x][scanline_data.bg_tile_y]
                if tile_attr.vertical_flip then
                    scanline_data.sub_y = 7 - scanline_data.sub_y
                end

                scanline_data.active_attr = scanline_data.current_map_attr[scanline_data.bg_tile_x]
                [scanline_data.bg_tile_y]
                scanline_data.active_tile = scanline_data.current_map[scanline_data.bg_tile_x][scanline_data.bg_tile_y]
            end
        end
    end

    graphics.getIndexFromTilemap = function(map, tile_data, x, y)
        local tile_x = bit32.rshift(x, 3)
        local tile_y = bit32.rshift(y, 3)
        local tile_index = map[tile_x][tile_y]

        local subpixel_x = x - (tile_x * 8)
        local subpixel_y = y - (tile_y * 8)

        if tile_data == 0x9000 then
            if tile_index > 127 then
                tile_index = tile_index - 256
            end
            -- add offset to re-root at tile 256 (so effectively, we read from tile 192 - 384)
            tile_index = tile_index + 256
        end

        if graphics.gameboy.type == graphics.gameboy.types.color then
            local map_attr = graphics.cache.map_0_attr
            if map == graphics.cache.map_1 then
                map_attr = graphics.cache.map_1_attr
            end
            local tile_attributes = map_attr[tile_x][tile_y]
            tile_index = tile_index + tile_attributes.bank * 384

            if tile_attributes.horizontal_flip == true then
                subpixel_x = (7 - subpixel_x)
            end

            if tile_attributes.vertical_flip == true then
                subpixel_y = (7 - subpixel_y)
            end
        end

        return graphics.cache.tiles[tile_index][subpixel_x][subpixel_y]
    end

    graphics.draw_sprites_into_scanline = function(scanline, bg_index, bg_priority)
        if not graphics.registers.sprites_enabled then
            return
        end
        local active_sprites = {}
        local sprite_size = 8
        if graphics.registers.large_sprites then
            sprite_size = 16
        end

        -- Collect up to the 10 highest priority sprites in a list.
        -- Sprites have priority first by their X coordinate, then by their index
        -- in the list.
        local i = 0
        while i < 40 do
            -- is this sprite being displayed on this scanline? (respect to Y coordinate)
            local sprite_y = graphics.cache.oam[i].y
            local sprite_lower = sprite_y
            local sprite_upper = sprite_y + sprite_size
            if scanline >= sprite_lower and scanline < sprite_upper then
                if #active_sprites < 10 then
                    table.insert(active_sprites, i)
                else
                    -- There are more than 10 sprites in the table, so we need to pick
                    -- a candidate to vote off the island (possibly this one)
                    local lowest_priority = i
                    local lowest_priotity_index = nil
                    for j = 1, #active_sprites do
                        local lowest_x = graphics.cache.oam[lowest_priority].x
                        local candidate_x = graphics.cache.oam[active_sprites[j]].x
                        if candidate_x > lowest_x then
                            lowest_priority = active_sprites[j]
                            lowest_priority_index = j
                        end
                    end
                    if lowest_priority_index then
                        active_sprites[lowest_priority_index] = i
                    end
                end
            end
            i = i + 1
        end

        -- now, for every sprite in the list, display it on the current scanline
        for i = #active_sprites, 1, -1 do
            local sprite = graphics.cache.oam[active_sprites[i]]

            local sub_y = 16 - ((sprite.y + 16) - scanline)
            if sprite.vertical_flip then
                sub_y = sprite_size - 1 - sub_y
            end

            local tile = sprite.tile
            if sprite_size == 16 then
                tile = sprite.upper_tile
                if sub_y >= 8 then
                    tile = sprite.lower_tile
                    sub_y = sub_y - 8
                end
            end

            local game_screen = graphics.game_screen

            for x = 0, 7 do
                local display_x = sprite.x + x
                if display_x >= 0 and display_x < 160 then
                    local sub_x = x
                    if x_flipped then
                        sub_x = 7 - x
                    end
                    local subpixel_index = tile[sub_x][sub_y]
                    if subpixel_index > 0 then
                        if (bg_priority[display_x] == false and not sprite.bg_priority) or bg_index[display_x] == 0 or graphics.registers.oam_priority then
                            local subpixel_color = sprite.palette[subpixel_index]
                            game_screen[scanline][display_x][1] = subpixel_color[1]
                            game_screen[scanline][display_x][2] = subpixel_color[2]
                            game_screen[scanline][display_x][3] = subpixel_color[3]
                        end
                    end
                end
            end
        end
        if #active_sprites > 0 then
        end
    end

    return graphics
end

return Graphics
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.graphics.cache" ] = function( ... ) local arg = _G.arg;
 local bit32 = require("vendor.bitop-lua").bit32

local Cache = {}

function Cache.new(graphics)
  local cache = {}

  cache.tiles = {}
  cache.tiles_h_flipped = {}
  cache.map_0 = {}
  cache.map_1 = {}
  cache.map_0_attr = {}
  cache.map_1_attr = {}
  cache.oam = {}

  cache.reset = function()
    for i = 0, 768 - 1 do
      cache.tiles[i] = {}
      cache.tiles_h_flipped[i] = {}
      for x = 0, 7 do
        cache.tiles[i][x] = {}
        cache.tiles_h_flipped[i][x] = {}
        for y = 0, 7 do
          cache.tiles[i][x][y] = 0
          cache.tiles_h_flipped[i][x][y] = 0
        end
      end
    end

    for x = 0, 31 do
      cache.map_0[x] = {}
      cache.map_1[x] = {}
      cache.map_0_attr[x] = {}
      cache.map_1_attr[x] = {}
      for y = 0, 31 do
        cache.map_0[x][y] = cache.tiles[0]
        cache.map_1[x][y] = cache.tiles[0]
        cache.map_0_attr[x][y] = {}
        cache.map_1_attr[x][y] = {}

        if graphics.gameboy.type == graphics.gameboy.types.color then
          cache.map_0_attr[x][y].palette = graphics.palette.color_bg[0]
        else
          cache.map_0_attr[x][y].palette = graphics.palette.bg
        end
        cache.map_0_attr[x][y].bank = 0
        cache.map_0_attr[x][y].horizontal_flip = false
        cache.map_0_attr[x][y].vertical_flip = false
        cache.map_0_attr[x][y].priority = false

        if graphics.gameboy.type == graphics.gameboy.types.color then
          cache.map_1_attr[x][y].palette = graphics.palette.color_bg[0]
        else
          cache.map_1_attr[x][y].palette = graphics.palette.bg
        end
        cache.map_1_attr[x][y].bank = 0
        cache.map_1_attr[x][y].horizontal_flip = false
        cache.map_1_attr[x][y].vertical_flip = false
        cache.map_1_attr[x][y].priority = false
      end
    end

    for i = 0, 39 do
      cache.oam[i] = {}
      cache.oam[i].x = 0
      cache.oam[i].y = 0
      cache.oam[i].tile = cache.tiles[0]
      cache.oam[i].upper_tile = cache.tiles[0]
      cache.oam[i].lower_tile = cache.tiles[1]
      cache.oam[i].bg_priority = false
      cache.oam[i].horizontal_flip = false
      cache.oam[i].vertical_flip = false
      if graphics.gameboy.type == graphics.gameboy.types.color then
        cache.oam[i].palette = graphics.palette.color_obj[0]
      else
        cache.oam[i].palette = graphics.palette.bg
      end
    end
  end

  cache.refreshOamEntry = function(index)
    local y = graphics.oam[0xFE00 + index * 4 + 0] - 16
    local x = graphics.oam[0xFE00 + index * 4 + 1] - 8
    local tile_index = graphics.oam[0xFE00 + index * 4 + 2]
    local flags = graphics.oam[0xFE00 + index * 4 + 3]

    cache.oam[index].x = x
    cache.oam[index].y = y
    local vram_bank = 0
    if graphics.gameboy.type == graphics.gameboy.types.color then
      vram_bank = bit32.rshift(bit32.band(0x08, flags), 3)
    end
    cache.oam[index].bg_priority = bit32.band(0x80, flags) ~= 0
    cache.oam[index].vertical_flip = bit32.band(0x40, flags) ~= 0
    cache.oam[index].horizontal_flip = bit32.band(0x20, flags) ~= 0
    if graphics.gameboy.type == graphics.gameboy.types.color then
      local palette_index = bit32.band(0x07, flags)
      cache.oam[index].palette = graphics.palette.color_obj[palette_index]
    else
      if bit32.band(0x10, flags) ~= 0 then
        cache.oam[index].palette = graphics.palette.obj1
      else
        cache.oam[index].palette = graphics.palette.obj0
      end
    end
    if cache.oam[index].horizontal_flip then
      cache.oam[index].tile =       cache.tiles_h_flipped[tile_index + (384 * vram_bank)]
      cache.oam[index].upper_tile = cache.tiles_h_flipped[bit32.band(tile_index, 0xFE) + (384 * vram_bank)]
      cache.oam[index].lower_tile = cache.tiles_h_flipped[bit32.band(tile_index, 0xFE) + 1 + (384 * vram_bank)]
    else
      cache.oam[index].tile =       cache.tiles[tile_index + (384 * vram_bank)]
      cache.oam[index].upper_tile = cache.tiles[bit32.band(tile_index, 0xFE) + (384 * vram_bank)]
      cache.oam[index].lower_tile = cache.tiles[bit32.band(tile_index, 0xFE) + 1 + (384 * vram_bank)]
    end

  end

  cache.refreshAttributes = function(map_attr, x, y, address)
    local data = graphics.vram[address + (16 * 1024)]
    if graphics.gameboy.type == graphics.gameboy.types.color then
      map_attr[x][y].palette = graphics.palette.color_bg[bit32.band(data, 0x07)]
    else
      map_attr[x][y].palette = graphics.palette.bg
    end
    map_attr[x][y].bank = bit32.rshift(bit32.band(data, 0x08), 3)
    map_attr[x][y].horizontal_flip = bit32.rshift(bit32.band(data, 0x20), 5) ~= 0
    map_attr[x][y].vertical_flip = bit32.rshift(bit32.band(data, 0x40), 6) ~= 0
    map_attr[x][y].priority = bit32.rshift(bit32.band(data, 0x80), 7) ~= 0
  end

  cache.refreshTile = function(address, bank)
    -- Update the cached tile data
    local tile_index = math.floor((address - 0x8000) / 16) + (384 * bank)
    local y = math.floor((address % 16) / 2)
    -- kill the lower bit
    address = bit32.band(address, 0xFFFE)
    local lower_bits = graphics.vram[address + (16 * 1024 * bank)]
    local upper_bits = graphics.vram[address + (16 * 1024 * bank) + 1]
    for x = 0, 7 do
      local palette_index = bit32.band(bit32.rshift(lower_bits, 7 - x), 0x1) + (bit32.band(bit32.rshift(upper_bits, 7 - x), 0x1) * 2)
      cache.tiles[tile_index][x][y] = palette_index
      cache.tiles_h_flipped[tile_index][7 - x][y] = palette_index
    end
  end

  cache.refreshTiles = function()
    for i = 0, 384 - 1 do
      for y = 0, 7 do
        cache.refreshTile(0x8000 + i * 16 + y * 2, 0)
        cache.refreshTile(0x8000 + i * 16 + y * 2, 1)
      end
    end
  end

  cache.refreshTileIndex = function(x, y, address, map, attr)
    local tile_index = graphics.vram[address + (y * 32) + x]
    if graphics.registers.tile_select == 0x9000 then
      if tile_index > 127 then
        tile_index = tile_index - 256
      end
      -- add offset to re-root at tile 256 (so effectively, we read from tile 192 - 384)
      tile_index = tile_index + 256
    end
    if attr[x][y].bank == 1 then
      tile_index = tile_index + 384
    end
    if attr[x][y].horizontal_flip then
      map[x][y] = cache.tiles_h_flipped[tile_index]
    else
      map[x][y] = cache.tiles[tile_index]
    end
  end

  cache.refreshTileMap = function(address, map, attr)
    for x = 0, 31 do
      for y = 0, 31 do
        cache.refreshTileIndex(x, y, address, map, attr)
      end
    end
  end

  cache.refreshTileMaps = function()
    cache.refreshTileMap(0x9800, cache.map_0, cache.map_0_attr)
    cache.refreshTileMap(0x9C00, cache.map_1, cache.map_1_attr)
  end

  cache.refreshTileAttributes = function()
    for x = 0, 31 do
      for y = 0, 31 do
        cache.refreshAttributes(cache.map_0_attr, x, y, 0x9800 + (y * 32) + x)
        cache.refreshAttributes(cache.map_1_attr, x, y, 0x9C00 + (y * 32) + x)
      end
    end
  end

  cache.refreshAll = function()
    cache.refreshTiles()
    cache.refreshTileAttributes()
    cache.refreshTileMaps()
  end

  return cache
end

return Cache
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.graphics.palette" ] = function( ... ) local arg = _G.arg;
 local bit32 = require("vendor.bitop-lua").bit32

local Palette = {}

function Palette.new(graphics, modules)
  local io = modules.io
  local ports = io.ports

  local palette = {}

  local dmg_colors = {}
  dmg_colors[0] = {255, 255, 255}
  dmg_colors[1] = {192, 192, 192}
  dmg_colors[2] = {128, 128, 128}
  dmg_colors[3] = {0, 0, 0}
  palette.dmg_colors = dmg_colors

  palette.set_dmg_colors = function(pal_0, pal_1, pal_2, pal_3)
    dmg_colors[0] = pal_0
    dmg_colors[1] = pal_1
    dmg_colors[2] = pal_2
    dmg_colors[3] = pal_3
  end

  palette.bg =   {}
  palette.obj0 = {}
  palette.obj1 = {}

  palette.color_bg = {}
  palette.color_obj = {}
  palette.color_bg_raw = {}
  palette.color_obj_raw = {}

  palette.reset = function()
    for i = 0, 3 do
      palette.bg[i] = dmg_colors[i]
      palette.obj0[i] = dmg_colors[i]
      palette.obj1[i] = dmg_colors[i]
    end

    for p = 0, 7 do
      palette.color_bg[p] = {}
      palette.color_obj[p] = {}
      for i = 0, 3 do
        palette.color_bg[p][i] = {255, 255, 255}
        palette.color_obj[p][i] = {255, 255, 255}
      end
    end

    for i = 0, 63 do
      palette.color_bg_raw[i] = 0
      palette.color_obj_raw[i] = 0
    end
  end

  palette.reset()

  local getColorFromIndex = function(index, palette)
    palette = palette or 0xE4
    while index > 0 do
      palette = bit32.rshift(palette, 2)
      index = index - 1
    end
    return dmg_colors[bit32.band(palette, 0x3)]
  end

  -- DMG palettes
  io.write_logic[ports.BGP] = function(byte)
    io.ram[ports.BGP] = byte
    for i = 0, 3 do
      palette.bg[i] = getColorFromIndex(i, byte)
    end
    graphics.update()
  end

  io.write_logic[ports.OBP0] = function(byte)
    io.ram[ports.OBP0] = byte
    for i = 0, 3 do
      palette.obj0[i] = getColorFromIndex(i, byte)
    end
    graphics.update()
  end

  io.write_logic[ports.OBP1] = function(byte)
    io.ram[ports.OBP1] = byte
    for i = 0, 3 do
      palette.obj1[i] = getColorFromIndex(i, byte)
    end
    graphics.update()
  end

  palette.color_bg_index = 0
  palette.color_bg_auto_increment = false
  palette.color_obj_index = 0
  palette.color_obj_auto_increment = false

  -- Color Palettes
  io.write_logic[0x68] = function(byte)
    io.ram[0x68] = byte
    palette.color_bg_index = bit32.band(byte, 0x3F)
    palette.color_bg_auto_increment = bit32.band(byte, 0x80) ~= 0
  end

  io.write_logic[0x69] = function(byte)
    palette.color_bg_raw[palette.color_bg_index] = byte

    -- Update the palette cache for this byte pair
    local low_byte = palette.color_bg_raw[bit32.band(palette.color_bg_index, 0xFE)]
    local high_byte = palette.color_bg_raw[bit32.band(palette.color_bg_index, 0xFE) + 1]
    local rgb5_color = bit32.lshift(high_byte, 8) + low_byte
    local r = bit32.band(rgb5_color, 0x001F) * 8
    local g = bit32.rshift(bit32.band(rgb5_color, 0x03E0), 5) * 8
    local b = bit32.rshift(bit32.band(rgb5_color, 0x7C00), 10) * 8
    local palette_index = math.floor(palette.color_bg_index / 8)
    local color_index = math.floor((palette.color_bg_index % 8) / 2)
    palette.color_bg[palette_index][color_index] = {r, g, b}

    if palette.color_bg_auto_increment then
      palette.color_bg_index = palette.color_bg_index + 1
      if palette.color_bg_index > 63 then
        palette.color_bg_index = 0
      end
    end
  end

  io.read_logic[0x69] = function()
    return palette.color_bg_raw[palette.color_bg_index]
  end

  io.write_logic[0x6A] = function(byte)
    io.ram[0x6A] = byte
    palette.color_obj_index = bit32.band(byte, 0x3F)
    palette.color_obj_auto_increment = bit32.band(byte, 0x80) ~= 0
  end

  io.write_logic[0x6B] = function(byte)
    palette.color_obj_raw[palette.color_obj_index] = byte

    -- Update the palette cache for this byte pair
    local low_byte = palette.color_obj_raw[bit32.band(palette.color_obj_index, 0xFE)]
    local high_byte = palette.color_obj_raw[bit32.band(palette.color_obj_index, 0xFE) + 1]
    local rgb5_color = bit32.lshift(high_byte, 8) + low_byte
    local r = bit32.band(rgb5_color, 0x001F) * 8
    local g = bit32.rshift(bit32.band(rgb5_color, 0x03E0), 5) * 8
    local b = bit32.rshift(bit32.band(rgb5_color, 0x7C00), 10) * 8
    local palette_index = math.floor(palette.color_obj_index / 8)
    local color_index = math.floor((palette.color_obj_index % 8) / 2)
    palette.color_obj[palette_index][color_index] = {r, g, b}

    if palette.color_obj_auto_increment then
      palette.color_obj_index = palette.color_obj_index + 1
      if palette.color_obj_index > 63 then
        palette.color_obj_index = 0
      end
    end
  end

  io.read_logic[0x6B] = function()
    return palette.color_obj_raw[palette.color_obj_index]
  end

  return palette
end

return Palette
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.graphics.registers" ] = function( ... ) local arg = _G.arg;
 local bit32 = require("vendor.bitop-lua").bit32

local Registers = {}

function Registers.new(graphics, modules, cache)
  local io = modules.io
  local ports = io.ports

  local registers = {}

  registers.display_enabled = true
  registers.window_tilemap = cache.map_0
  registers.window_attr = cache.map_0_attr
  registers.window_enabled = true
  registers.tile_select = 0x9000
  registers.background_tilemap = cache.map_0
  registers.background_attr = cache.map_0_attr
  registers.large_sprites = false
  registers.sprites_enabled = true
  registers.background_enabled = true
  registers.oam_priority = false

  io.write_logic[ports.LCDC] = function(byte)
    io.ram[ports.LCDC] = byte

    -- Unpack all the bit flags into lua variables, for great sanity
    registers.display_enabled = bit32.band(0x80, byte) ~= 0
    registers.window_enabled  = bit32.band(0x20, byte) ~= 0
    registers.large_sprites   = bit32.band(0x04, byte) ~= 0
    registers.sprites_enabled = bit32.band(0x02, byte) ~= 0

    if graphics.gameboy.type == graphics.gameboy.types.color then
      registers.oam_priority = bit32.band(0x01, byte) == 0
    else
      registers.background_enabled = bit32.band(0x01, byte) ~= 0
    end

    if bit32.band(0x40, byte) ~= 0 then
      registers.window_tilemap = cache.map_1
      registers.window_attr = cache.map_1_attr
    else
      registers.window_tilemap = cache.map_0
      registers.window_attr = cache.map_0_attr
    end

    if bit32.band(0x10, byte) ~= 0 then
      if registers.tile_select == 0x9000 then
        -- refresh our tile indices, they'll all need recalculating for the new offset
        registers.tile_select = 0x8000
        cache.refreshTileMaps()
      end
    else
      if registers.tile_select == 0x8000 then
        -- refresh our tile indices, they'll all need recalculating for the new offset
        registers.tile_select = 0x9000
        cache.refreshTileMaps()
      end
    end

    if bit32.band(0x08, byte) ~= 0 then
      registers.background_tilemap = cache.map_1
      registers.background_attr = cache.map_1_attr
    else
      registers.background_tilemap = cache.map_0
      registers.background_attr = cache.map_0_attr
    end
  end

  local status = {}
  registers.status = status

  status.mode = 2

  status.SetMode = function(mode)
    status.mode = mode
    io.ram[ports.STAT] = bit32.band(io.ram[ports.STAT], 0xFC) + bit32.band(mode, 0x3)
  end

  status.lyc_interrupt_enabled = false
  status.oam_interrupt_enabled = false
  status.vblank_interrupt_enabled = false
  status.hblank_interrupt_enabled = false

  io.write_logic[ports.STAT] = function(byte)
    io.ram[ports.STAT] = bit32.band(byte, 0x78)
    status.lyc_interrupt_enabled = bit32.band(byte, 0x40) ~= 0
    status.oam_interrupt_enabled = bit32.band(byte, 0x20) ~= 0
    status.vblank_interrupt_enabled = bit32.band(byte, 0x10) ~= 0
    status.hblank_interrupt_enabled = bit32.band(byte, 0x08) ~= 0
  end

  return registers
end

return Registers
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.init" ] = function( ... ) local arg = _G.arg;
local bit32 = require("vendor.bitop-lua").bit32

local Gameboy = {}

Gameboy.audio = require("gameboy.audio")
Gameboy.cartridge = require("gameboy.cartridge")
Gameboy.dma = require("gameboy.dma")
Gameboy.graphics = require("gameboy.graphics")
Gameboy.input = require("gameboy.input")
Gameboy.interrupts = require("gameboy.interrupts")
Gameboy.io = require("gameboy.io")
Gameboy.memory = require("gameboy.memory")
Gameboy.timers = require("gameboy.timers")
Gameboy.processor = require("gameboy.z80")

function Gameboy:initialize()
    self.audio.initialize()
    self.graphics.initialize(self)
    self.cartridge.initialize(self)

    self:reset()
end

Gameboy.types = {}
Gameboy.types.dmg = 0
Gameboy.types.sgb = 1
Gameboy.types.color = 2

Gameboy.type = Gameboy.types.color

function Gameboy:reset()
    -- Resets the gameboy's internal state to just after the power-on and boot sequence
    -- (Does NOT unload the cartridge)

    -- Note: IO needs to come first here, as some subsequent modules
    -- manipulate IO registers during reset / initialization
    self.audio.reset()
    self.io.reset(self)
    self.memory.reset()
    self.cartridge.reset()
    self.graphics.reset() -- Note to self: this needs to come AFTER resetting IO
    self.timers:reset()
    self.processor.reset(self)

    self.interrupts.enabled = 1
end

function Gameboy:save_state()
    local state = {}
    state.audio = self.audio.save_state()
    state.cartridge = self.cartridge.save_state()
    state.io = self.io.save_state()
    state.memory = self.memory.save_state()
    state.graphics = self.graphics.save_state()
    state.timers = self.timers:save_state()
    state.processor = self.processor.save_state()

    -- Note: the underscore
    state.interrupts_enabled = self.interrupts.enabled
    return state
end

function Gameboy:load_state(state)
    self.audio.load_state(state.audio)
    self.cartridge.load_state(state.cartridge)
    self.io.load_state(state.io)
    self.memory.load_state(state.memory)
    self.graphics.load_state(state.graphics)
    self.timers:load_state(state.timers)
    self.processor.load_state(state.processor)

    -- Note: the underscore
    self.interrupts.enabled = state.interrupts_enabled
end

function Gameboy:step()
    self.timers:update()
    if self.timers.system_clock > self.graphics.next_edge then
        self.graphics.update()
    end
    self.processor.process_instruction()
end

function Gameboy:run_until_vblank()
    local instructions = 0
    while self.io.ram[self.io.ports.LY] == 144 and instructions < 100000 do
        self:step()
        instructions = instructions + 1
    end
    while self.io.ram[self.io.ports.LY] ~= 144 and instructions < 100000 do
        self:step()
        instructions = instructions + 1
    end
    self.audio.update()
end

function Gameboy:run_until_hblank()
    local old_scanline = self.io.ram[self.io.ports.LY]
    local instructions = 0
    while old_scanline == self.io.ram[self.io.ports.LY] and instructions < 100000 do
        self:step()
        instructions = instructions + 1
    end
    self.audio.update()
end

local call_opcodes = { [0xCD] = true, [0xC4] = true, [0xD4] = true, [0xCC] = true, [0xDC] = true }
local rst_opcodes = {
    [0xC7] = true,
    [0xCF] = true,
    [0xD7] = true,
    [0xDF] = true,
    [0xE7] = true,
    [0xEF] = true,
    [0xF7] = true,
    [0xFF] = true
}

function Gameboy:step_over()
    -- Make sure the *current* opcode is a CALL / RST
    local instructions = 0
    local pc = self.processor.registers.pc
    local opcode = self.memory[pc]
    if call_opcodes[opcode] then
        local return_address = bit32.band(pc + 3, 0xFFFF)
        while self.processor.registers.pc ~= return_address and instructions < 10000000 do
            self:step()
            instructions = instructions + 1
        end
        return
    end
    if rst_opcodes[opcode] then
        local return_address = bit32.band(pc + 1, 0xFFFF)
        while self.processor.registers.pc ~= return_address and instructions < 10000000 do
            self:step()
            instructions = instructions + 1
        end
        return
    end
    print("Not a CALL / RST opcode! Bailing.")
end

local ret_opcodes = { [0xC9] = true, [0xC0] = true, [0xD0] = true, [0xC8] = true, [0xD8] = true, [0xD9] = true }

function Gameboy:run_until_ret()
    local instructions = 0
    while ret_opcodes[self.memory[self.processor.registers.pc]] ~= true and instructions < 10000000 do
        self:step()
        instructions = instructions + 1
    end
end

local gameboy_defaults = {}
for k, v in pairs(Gameboy) do
    gameboy_defaults[k] = v
end

Gameboy.new = function(overrides)
    local new_gameboy = {}
    for k, v in pairs(gameboy_defaults) do
        if overrides[k] then
            new_gameboy[k] = overrides[k]
        else
            new_gameboy[k] = gameboy_defaults[k]
        end
    end

    new_gameboy.memory = new_gameboy.memory.new(new_gameboy)

    new_gameboy.io = new_gameboy.io.new(new_gameboy)

    new_gameboy.interrupts = new_gameboy.interrupts.new(new_gameboy)

    new_gameboy.timers = new_gameboy.timers.new(new_gameboy)

    new_gameboy.audio = new_gameboy.audio.new(new_gameboy)
    new_gameboy.cartridge = new_gameboy.cartridge.new(new_gameboy)
    new_gameboy.dma = new_gameboy.dma.new(new_gameboy)
    new_gameboy.graphics = new_gameboy.graphics.new(new_gameboy)
    new_gameboy.input = new_gameboy.input.new(new_gameboy)
    new_gameboy.processor = new_gameboy.processor.new(new_gameboy)

    new_gameboy:initialize()

    return new_gameboy
end

return Gameboy
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.input" ] = function( ... ) local arg = _G.arg;
 local bit32 = require("vendor.bitop-lua").bit32

local Input = {}

function Input.new(modules)
  local memory = modules.memory
  local io = modules.io

  local input = {}

  input.keys = {}
  input.keys.Left = 0
  input.keys.Right = 0
  input.keys.Up = 0
  input.keys.Down = 0
  input.keys.A = 0
  input.keys.B = 0
  input.keys.Start = 0
  input.keys.Select = 0

  input.update = function()
    local d_pad_bits = input.keys.Right +
                 bit32.lshift(input.keys.Left, 1) +
                 bit32.lshift(input.keys.Up, 2) +
                 bit32.lshift(input.keys.Down, 3)
    local button_bits = input.keys.A +
                  bit32.lshift(input.keys.B, 1) +
                  bit32.lshift(input.keys.Select, 2) +
                  bit32.lshift(input.keys.Start, 3)

    local active_bits = 0
    if bit32.band(io.ram[io.ports.JOYP], 0x20) == 0 then
      active_bits = bit32.bor(active_bits, button_bits)
    end
    if bit32.band(io.ram[io.ports.JOYP], 0x10) == 0 then
      active_bits = bit32.bor(active_bits, d_pad_bits)
    end
    active_bits = bit32.bnot(active_bits)

    io.ram[io.ports.JOYP] = bit32.bor(0xC0, bit32.band(io.ram[io.ports.JOYP], 0x30), bit32.band(active_bits, 0x0F))
  end

  local snes_packet_names = {}
  snes_packet_names[0x00] = "PAL 01"
  snes_packet_names[0x01] = "PAL 23"
  snes_packet_names[0x02] = "PAL 03"
  snes_packet_names[0x03] = "PAL 12"
  snes_packet_names[0x04] = "ATTR_BLK"
  snes_packet_names[0x05] = "ATTR_LIN"
  snes_packet_names[0x06] = "ATTR_DIV"
  snes_packet_names[0x07] = "ATTR_CHR"
  snes_packet_names[0x08] = "SOUND"
  snes_packet_names[0x09] = "SOU_TRN"
  snes_packet_names[0x0A] = "PAL_SET"
  snes_packet_names[0x0B] = "PAL_TRN"
  snes_packet_names[0x0C] = "ATRC_EN"
  snes_packet_names[0x0D] = "TEST_EN"
  snes_packet_names[0x0E] = "ICON_EN"
  snes_packet_names[0x0F] = "DATA_SND"
  snes_packet_names[0x10] = "DATA_TRN"
  snes_packet_names[0x11] = "MLT_REG"
  snes_packet_names[0x12] = "JUMP"
  snes_packet_names[0x13] = "CHR_TRN"
  snes_packet_names[0x14] = "PCT_TRN"
  snes_packet_names[0x15] = "ATTR_TRN"
  snes_packet_names[0x16] = "ATTR_SET"
  snes_packet_names[0x17] = "MASK_EN"
  snes_packet_names[0x18] = "OBJ_TRN"

  local decode_snes_command = function(command_bits)
    local command_bytes = {}
    for i = 0, 15 do
      command_bytes[i] = 0
      for b = 0, 7 do
        command_bytes[i] = command_bytes[i] + bit32.lshift(command_bits[8 * i + b], 8)
        command_bytes[i] = bit32.rshift(command_bytes[i], 1)

      end
    end

    local command = bit32.rshift(bit32.band(command_bytes[0], 0xF8), 3)
    local packet_length = bit32.band(command_bytes[0], 0x7)
    local parameters = {}
    for i = 1, 15 do
      parameters[i] = command_bytes[i]
    end
    return command, packet_length, parameters
  end

  local last_write = 0
  local command_bits = {}
  local command_started = false
  local command_index = 0

  local hex = function(str)
    return string.format("$%02X", str)
  end

  -- Register hooks for input-related registers
  io.write_logic[io.ports.JOYP] = function(byte)
    io.ram[io.ports.JOYP] = bit32.band(byte, 0x30)
    input.update()

    local pulse = bit32.rshift(bit32.band(byte, 0x30), 4)
    if command_started then
      if (pulse == 0x1 or pulse == 0x2) and last_write == 0x3 then
        if pulse == 0x2 then
          command_bits[command_index] = 0
        end
        if pulse == 0x1 then
          command_bits[command_index] = 1
        end
        command_index = command_index + 1
        if command_index > 128 then
          if command_bits[128] ~= 0 then
            print("Invalid command! 129th bit was not 0")
          end
          local command, length, parameters = decode_snes_command(command_bits)
          local command_name = snes_packet_names[command] or "UNKNOWN!!"
          print("SNES Command: " .. command_name .. " [" .. hex(command) .. "] Length: " .. length)
          local hex_params = hex(parameters[1])
          for i = 2, 15 do
            hex_params = hex_params .. " " .. hex(parameters[i])
          end
          print("SNES Parameters: ", hex_params)
          command_started = false
        end
      end
    else
      -- Check to see if we are starting a new command
      if pulse == 0x3 and last_write == 0x0 then
        command_started = true
        command_index = 0
      end
    end

    last_write = pulse
  end

  return input
end

return Input
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.interrupts" ] = function( ... ) local arg = _G.arg;
 local bit32 = require("vendor.bitop-lua").bit32

local Interrupts = {}

function Interrupts.new(modules)
  local interrupts = {}

  local io = modules.io

  interrupts.VBlank = 0x1
  interrupts.LCDStat = 0x2
  interrupts.Timer = 0x4
  interrupts.Serial = 0x8
  interrupts.Joypad = 0x16

  interrupts.enabled = 1

  interrupts.service_handler = function() end

  interrupts.enable = function()
    interrupts.enabled = 1
  end

  interrupts.disable = function()
    interrupts.enabled = 0
  end

  function interrupts.raise(bitmask)
    io.ram[0x0F] = bit32.band(bit32.bor(io.ram[0x0F], bitmask), 0x1F)
    interrupts.service_handler()
  end

  io.write_logic[io.ports.IF] = function(byte)
    io.ram[io.ports.IF] = byte
    if byte ~= 0 then
      interrupts.service_handler()
    end
  end

  io.write_logic[io.ports.IE] = function(byte)
    io.ram[io.ports.IE] = byte
    if byte ~= 0 then
      interrupts.service_handler()
    end
  end

  return interrupts
end

return Interrupts
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.io" ] = function( ... ) local arg = _G.arg;
 local bit32 = require("vendor.bitop-lua").bit32

local Io = {}

function Io.new(modules)
  local io = {}

  local memory = modules.memory

  local ports = {}
  -- Port names pulled from Pan Docs, starting here:
  -- http://bgb.bircd.org/pandocs.htm#videodisplay

  -- LCD Control
  ports.LCDC = 0x40

  -- LCD Status
  ports.STAT = 0x41

  -- BG Scroll
  ports.SCY = 0x42
  ports.SCX = 0x43

  -- Current Scanline (LCDC Y-coordinate)
  ports.LY = 0x44
  -- LCD Compare, scanline on which a STAT interrupt is requested
  ports.LYC = 0x45

  -- B&W Palettes
  ports.BGP = 0x47
  ports.OBP0 = 0x48
  ports.OBP1 = 0x49

  -- Window Scroll
  ports.WY = 0x4A
  ports.WX = 0x4B

  -- Color-mode Palettes
  ports.BGPI = 0x68
  ports.BGPD = 0x69
  ports.OBPI = 0x6A
  ports.OBPD = 0x6B

  -- Color-mode VRAM Bank
  ports.VBK = 0x4F

  -- DMA Transfer Start (Write Only)
  ports.DMA = 0x46

  -- Joypad
  ports.JOYP = 0x00

  -- Timers
  ports.DIV = 0x04
  ports.TIMA = 0x05
  ports.TMA = 0x06
  ports.TAC = 0x07

  -- Interrupts
  ports.IE = 0xFF
  ports.IF = 0x0F

  -- Sound
  ports.NR10 = 0x10
  ports.NR11 = 0x11
  ports.NR12 = 0x12
  ports.NR13 = 0x13
  ports.NR14 = 0x14

  ports.NR21 = 0x16
  ports.NR22 = 0x17
  ports.NR23 = 0x18
  ports.NR24 = 0x19

  ports.NR30 = 0x1A
  ports.NR31 = 0x1B
  ports.NR32 = 0x1C
  ports.NR33 = 0x1D
  ports.NR34 = 0x1E

  ports.NR41 = 0x20
  ports.NR42 = 0x21
  ports.NR43 = 0x22
  ports.NR44 = 0x23

  ports.NR50 = 0x24
  ports.NR51 = 0x25
  ports.NR52 = 0x26

  io.ports = ports


  io.write_logic = {}
  io.read_logic = {}
  io.write_mask = {}

  io.ram = memory.generate_block(0x100)
  io.block = {}
  io.block.mt = {}
  io.block.mt.__index = function(table, address)
    address = address - 0xFF00
    if io.read_logic[address] then
      return io.read_logic[address]()
    else
      return io.ram[address]
    end
  end

  io.write_mask[ports.JOYP] = 0x30
  io.write_mask[ports.LY] = 0x00

  io.write_logic[0x70] = function(byte)
    if io.gameboy.type == io.gameboy.types.color then
      io.ram[0x70] = bit32.band(0x7, byte)
      memory.work_ram_1.bank = bit32.band(0x7, byte)
      if memory.work_ram_1.bank == 0 then
        memory.work_ram_1.bank = 1
      end
    else
      -- Not sure if the write mask should apply in DMG / SGB mode
      io.ram[0x70] = byte
    end
  end

  io.block.mt.__newindex = function(table, address, value)
    address = address - 0xFF00
    if io.write_mask[address] then
      local masked_value = bit32.band(value, io.write_mask[address])
      local masked_memory = bit32.band(io.ram[address], bit32.bnot(io.write_mask[address]))
      value = masked_value + masked_memory
    end
    if io.write_logic[address] then
      -- Some addresses (mostly IO ports) have fancy logic or do strange things on
      -- writes, so we handle those here.
      io.write_logic[address](value)
      return
    end
    io.ram[address] = value
  end

  io.reset = function(gameboy)
    io.gameboy = gameboy

    for i = 0, #io.ram do
      io.ram[i] = 0
    end

    -- Set io registers to post power-on values
    -- Sound Enable must be set to F1
    io.ram[0x26] = 0xF1

    io.ram[ports.LCDC] = 0x91
    io.ram[ports.BGP ] = 0xFC
    io.ram[ports.OBP0] = 0xFF
    io.ram[ports.OBP1] = 0xFF
  end

  io.save_state = function()
    local state = {}

    for i = 0, 0xFF do
      state[i] = io.ram[i]
    end

    return state
  end

  io.load_state = function(state)
    for i = 0, 0xFF do
      io.ram[i] = state[i]
    end
  end

  setmetatable(io.block, io.block.mt)
  memory.map_block(0xFF, 0xFF, io.block, 0)

  return io
end

return Io
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.mbc.mbc1" ] = function( ... ) local arg = _G.arg;
local bit32 = require("vendor.bitop-lua").bit32

local Mbc1 = {}

function Mbc1.new()
    local mbc1 = {}
    mbc1.raw_data = {}
    mbc1.external_ram = {}
    mbc1.header = {}
    mbc1.rom_bank = 1
    mbc1.ram_bank = 0
    mbc1.mode = 0 --0 = ROM bank mode, 1 = RAM bank mode
    mbc1.ram_enable = false
    mbc1.mt = {}
    mbc1.mt.__index = function(table, address)
        -- Lower 16k: return the first bank, always
        if address <= 0x3FFF then
            return mbc1.raw_data[address]
        end
        -- Upper 16k: return the currently selected bank
        if address >= 0x4000 and address <= 0x7FFF then
            local rom_bank = mbc1.rom_bank
            if mbc1.mode == 0 then
                rom_bank = rom_bank + bit32.lshift(mbc1.ram_bank, 5)
            end
            return mbc1.raw_data[(rom_bank * 16 * 1024) + (address - 0x4000)]
        end

        if address >= 0xA000 and address <= 0xBFFF and mbc1.ram_enable then
            local ram_bank = 0
            if mbc1.mode == 1 then
                ram_bank = mbc1.ram_bank
            end
            return mbc1.external_ram[(address - 0xA000) + (ram_bank * 8 * 1024)]
        end

        return 0x00
    end
    mbc1.mt.__newindex = function(table, address, value)
        if address <= 0x1FFF then
            if bit32.band(0x0A, value) == 0x0A then
                mbc1.ram_enable = true
            else
                mbc1.ram_enable = false
            end
            return
        end
        if address >= 0x2000 and address <= 0x3FFF then
            -- Select the lower 5 bits of the ROM bank
            -- HARDWARE BUG: bank 0 is translated into bank 1 for weird reasons
            value = bit32.band(value, 0x1F)
            if value == 0 then
                value = 1
            end
            mbc1.rom_bank = value
            return
        end
        if address >= 0x4000 and address <= 0x5FFF then
            mbc1.ram_bank = bit32.band(value, 0x03)
            return
        end
        if address >= 0x6000 and address <= 0x7FFF then
            mbc1.mode = bit32.band(value, 0x01)
            return
        end

        -- Handle actually writing to External RAM
        if address >= 0xA000 and address <= 0xBFFF and mbc1.ram_enable then
            local ram_bank = 0
            if mbc1.mode == 1 then
                ram_bank = mbc1.ram_bank
            end
            mbc1.external_ram[(address - 0xA000) + (ram_bank * 8 * 1024)] = value
            mbc1.external_ram.dirty = true
            return
        end
    end

    mbc1.reset = function(self)
        self.rom_bank = 1
        self.ram_bank = 0
        self.mode = 0
        self.ram_enable = false
    end

    mbc1.save_state = function(self)
        return {
            rom_bank = self.rom_bank,
            ram_bank = self.ram_bank,
            mode = self.mode,
            ram_enable = self.ram_enable
        }
    end

    mbc1.load_state = function(self, state_data)
        self:reset()

        self.rom_bank = state_data.rom_bank
        self.ram_bank = state_data.ram_bank
        self.mode = state_data.mode
        self.ram_enable = state_data.ram_enable
    end

    setmetatable(mbc1, mbc1.mt)

    return mbc1
end

return Mbc1
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.mbc.mbc2" ] = function( ... ) local arg = _G.arg;
 local bit32 = require("vendor.bitop-lua").bit32

local Mbc2 = {}

function Mbc2.new()
  local mbc2 = {}
  mbc2.raw_data = {}
  mbc2.external_ram = {}
  mbc2.header = {}
  mbc2.rom_bank = 1
  mbc2.ram_enable = false
  mbc2.mt = {}
  mbc2.mt.__index = function(table, address)
    -- Lower 16k: return the first bank, always
    if address <= 0x3FFF then
      return mbc2.raw_data[address]
    end
    -- Upper 16k: return the currently selected bank
    if address >= 0x4000 and address <= 0x7FFF then
      local rom_bank = mbc2.rom_bank
      return mbc2.raw_data[(rom_bank * 16 * 1024) + (address - 0x4000)]
    end

    if address >= 0xA000 and address <= 0xA1FF and mbc2.ram_enable then
      -- For MBC2, only the lower 4 bits of each RAM byte are available for use
      return bit32.band(0x0F, mbc2.external_ram[(address - 0xA000)])
    end

    return 0x00
  end
  mbc2.mt.__newindex = function(table, address, value)
    if address <= 0x1FFF and bit32.band(address, 0x0100) == 0 then
      if bit32.band(0x0A, value) == 0x0A then
        mbc2.ram_enable = true
      else
        mbc2.ram_enable = false
      end
      return
    end
    if address >= 0x2000 and address <= 0x3FFF and bit32.band(address, 0x0100) ~= 0 then
      -- Select the ROM bank (4 bits)
      value = bit32.band(value, 0x0F)
      if value == 0 then
        value = 1
      end
      mbc2.rom_bank = value
      return
    end

    -- Handle actually writing to External RAM
    if address >= 0xA000 and address <= 0xBFFF and mbc2.ram_enable then
      mbc2.external_ram[(address - 0xA000)] = bit32.band(0x0F, value)
      mbc2.external_ram.dirty = true
      return
    end
  end

  mbc2.reset = function(self)
    self.rom_bank = 1
    self.ram_enable = false
  end

  mbc2.save_state = function(self)
    return {rom_bank = self.rom_bank, ram_enable = self.ram_enable}
  end

  mbc2.load_state = function(self, state_data)
    self:reset()

    self.rom_bank = state_data.rom_bank
    self.ram_enable = state_data.ram_enable
  end

  setmetatable(mbc2, mbc2.mt)

  return mbc2
end

return Mbc2
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.mbc.mbc3" ] = function( ... ) local arg = _G.arg;
 local bit32 = require("vendor.bitop-lua").bit32

local Mbc3 = {}

function Mbc3.new()
  local mbc3 = {}
  mbc3.raw_data = {}
  mbc3.external_ram = {}
  mbc3.header = {}
  mbc3.rom_bank = 0
  mbc3.ram_bank = 0
  mbc3.ram_enable = false
  mbc3.rtc_enable = false
  mbc3.rtc_select = 0x08
  mbc3.rtc = {}
  mbc3.rtc[0x08] = 0
  mbc3.rtc[0x09] = 0
  mbc3.rtc[0x0A] = 0
  mbc3.rtc[0x0B] = 0
  mbc3.rtc[0x0C] = 0
  mbc3.mt = {}
  mbc3.mt.__index = function(table, address)
    -- Lower 16k: return the first bank, always
    if address <= 0x3FFF then
      return mbc3.raw_data[address]
    end
    -- Upper 16k: return the currently selected bank
    if address >= 0x4000 and address <= 0x7FFF then
      local rom_bank = mbc3.rom_bank
      return mbc3.raw_data[(rom_bank * 16 * 1024) + (address - 0x4000)]
    end

    if address >= 0xA000 and address <= 0xBFFF and mbc3.ram_enable then
      if mbc3.rtc_enable then
        return mbc3.rtc[mbc3.rtc_select]
      else
        local ram_bank = mbc3.ram_bank
        return mbc3.external_ram[(address - 0xA000) + (ram_bank * 8 * 1024)]
      end
    end
    return 0x00
  end
  mbc3.mt.__newindex = function(table, address, value)
    if address <= 0x1FFF then
      if bit32.band(0x0A, value) == 0x0A then
        mbc3.ram_enable = true
      else
        mbc3.ram_enable = false
      end
      return
    end
    if address >= 0x2000 and address <= 0x3FFF then
      -- Select the lower 7 bits of the ROM bank
      value = bit32.band(value, 0x7F)
      if value == 0 then
        value = 1
      end
      mbc3.rom_bank = value
      return
    end
    if address >= 0x4000 and address <= 0x5FFF then
      mbc3.rtc_enable = false
      if value <= 0x03 then
        mbc3.ram_bank = bit32.band(value, 0x03)
        return
      end
      if value >= 0x08 and value <= 0x0C then
        mbc3.rtc_enable = true
        mbc3.rtc_select = value
        return
      end
    end
    if address >= 0x6000 and address <= 0x7FFF then
      -- Would "latch" the RTC registers, not implemented
      return
    end

    -- Handle actually writing to External RAM
    if address >= 0xA000 and address <= 0xBFFF and mbc3.ram_enable then
      local ram_bank = mbc3.ram_bank
      mbc3.external_ram[(address - 0xA000) + (ram_bank * 8 * 1024)] = value
      mbc3.external_ram.dirty = true
      return
    end
  end

  mbc3.reset = function(self)
    self.rom_bank = 1
    self.ram_bank = 0
    self.ram_enable = false
    self.rtc_enable = false
    self.rtc_select = 0x08
  end

  mbc3.save_state = function(self)
    return {
      rom_bank = self.rom_bank,
      ram_bank = self.ram_bank,
      ram_enable = self.ram_enable,
      rtc_enable = self.rtc_enable,
      rtc_select = self.rtc_enable}
  end

  mbc3.load_state = function(self, state_data)
    self:reset()

    self.rom_bank = state_data.rom_bank
    self.ram_bank = state_data.ram_bank
    self.ram_enable = state_data.ram_enable
    self.rtc_enable = state_data.rtc_enable
    self.rtc_select = state_data.rtc_select
  end

  setmetatable(mbc3, mbc3.mt)

  return mbc3
end

return Mbc3
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.mbc.mbc5" ] = function( ... ) local arg = _G.arg;
 local bit32 = require("vendor.bitop-lua").bit32

local Mbc5 = {}

function Mbc5.new()
  local mbc5 = {}
  mbc5.raw_data = {}
  mbc5.external_ram = {}
  mbc5.header = {}
  mbc5.rom_bank = 0
  mbc5.ram_bank = 0
  mbc5.ram_enable = false
  mbc5.rumble_pak = false
  mbc5.rumbling = false
  mbc5.mt = {}
  mbc5.mt.__index = function(table, address)
    -- Lower 16k: return the first bank, always
    if address <= 0x3FFF then
      return mbc5.raw_data[address]
    end
    -- Upper 16k: return the currently selected bank
    if address >= 0x4000 and address <= 0x7FFF then
      local rom_bank = mbc5.rom_bank
      return mbc5.raw_data[(rom_bank * 16 * 1024) + (address - 0x4000)]
    end

    if address >= 0xA000 and address <= 0xBFFF and mbc5.ram_enable then
      local ram_bank = mbc5.ram_bank
      return mbc5.external_ram[(address - 0xA000) + (ram_bank * 8 * 1024)]
    end
    return 0x00
  end
  mbc5.mt.__newindex = function(table, address, value)
    if address <= 0x1FFF then
      if bit32.band(0x0A, value) == 0x0A then
        mbc5.ram_enable = true
      else
        mbc5.ram_enable = false
      end
      return
    end
    if address >= 0x2000 and address <= 0x2FFF then
      -- Write the lower 8 bits of the ROM bank
      mbc5.rom_bank = bit32.band(mbc5.rom_bank, 0xFF00) + value
      return
    end
    if address >= 0x3000 and address <= 0x3FFF then
      if mbc5.header.rom_size > (4096 * 1024) then
        -- This is a >4MB game, so set the high bit of the bank select
        mbc5.rom_bank = bit32.band(mbc5.rom_bank, 0xFF) + bit32.lshift(bit32.band(value, 0x01), 8)
      else
        -- This is a <= 4MB game. Do nothing!
      end
      return
    end
    if address >= 0x4000 and address <= 0x5FFF then
      local ram_mask = 0x0F
      if mbc5.rumble_pak then
        ram_mask = 0x7
      end
      mbc5.ram_bank = bit32.band(value, ram_mask)
      if bit32.band(value, 0x08) ~= 0 and mbc5.rumbling == false then
        --print("Rumble on!")
        mbc5.rumbling = true
      end
      if bit32.band(value, 0x08) ~= 0 and mbc5.rumbling == true then
        --print("Rumble off!")
        mbc5.rumbling = false
      end
      return
    end

    -- Handle actually writing to External RAM
    if address >= 0xA000 and address <= 0xBFFF and mbc5.ram_enable then
      local ram_bank = mbc5.ram_bank
      mbc5.external_ram[(address - 0xA000) + (ram_bank * 8 * 1024)] = value
      mbc5.external_ram.dirty = true
      return
    end
  end

  mbc5.reset = function(self)
    self.rom_bank = 1
    self.ram_bank = 0
    self.ram_enable = false
  end

  mbc5.save_state = function(self)
    return {
      rom_bank = self.rom_bank,
      ram_bank = self.ram_bank,
      ram_enable = self.ram_enable,
      rumble_pak = self.rumble_pak}
  end

  mbc5.load_state = function(self, state_data)
    self:reset()

    self.rom_bank = state_data.rom_bank
    self.ram_bank = state_data.ram_bank
    self.ram_enable = state_data.ram_enable
    self.rumble_pak = state_data.rumble_pak
    self.rumbling = false
  end

  setmetatable(mbc5, mbc5.mt)

  return mbc5
end

return Mbc5
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.mbc.none" ] = function( ... ) local arg = _G.arg;
local MbcNone = {}

function MbcNone.new()
  -- Very simple: Map the entire 32k cartridge into lower memory, and be done with it.
  local mbc_none = {}
  mbc_none.mt = {}
  mbc_none.raw_data = {}
  mbc_none.external_ram = {}
  mbc_none.header = {}
  mbc_none.mt.__index = function(table, address)
    return mbc_none.raw_data[address]
  end
  mbc_none.mt.__newindex = function(table, address, value)
    --do nothing!
    return
  end

  mbc_none.load_state = function(self)
    -- Do nothing! This MBC has no state.
  end

  mbc_none.save_state = function(self)
    -- Return nothing! No state to save with this MBC.
    return nil
  end

  mbc_none.reset = function(self, state)
    -- Do nothing! This MBC has no state.
  end

  setmetatable(mbc_none, mbc_none.mt)

  return mbc_none
end

return MbcNone
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.memory" ] = function( ... ) local arg = _G.arg;
 local bit32 = require("vendor.bitop-lua").bit32

local Memory = {}

function Memory.new(modules)
  local memory = {}

  local block_map = {}
  memory.block_map = block_map

  memory.print_block_map = function()
    --debug
    print("Block Map: ")
    for b = 0, 0xFF do
      if block_map[bit32.lshift(b, 8)] then
        --print(string.format("Block at: %02X starts at %04X", b, block_map[bit32.lshift(b, 8)].start))
        print(block_map[bit32.lshift(b, 8)])
      end
    end
  end

  memory.map_block = function(starting_high_byte, ending_high_byte, mapped_block, starting_address)
    if starting_high_byte > 0xFF or ending_high_byte > 0xFF then
      print("Bad block, bailing", starting_high_byte, ending_high_byte)
      return
    end

    --starting_address = starting_address or bit32.lshift(starting_high_byte, 8)
    for i = starting_high_byte, ending_high_byte do
      --block_map[bit32.lshift(i, 8)] = {start=starting_address, block=mapped_block}
      block_map[bit32.lshift(i, 8)] = mapped_block
    end
  end

  memory.generate_block = function(size, starting_address)
    starting_address = starting_address or 0
    local block = {}
    for i = 0, size - 1 do
      block[starting_address + i] = 0
    end
    return block
  end

  -- Default, unmapped memory
  memory.unmapped = {}
  memory.unmapped.mt = {}
  memory.unmapped.mt.__index = function(table, key)
    return 0x00
  end
  memory.unmapped.mt.__newindex = function(table, key, value)
    -- Do nothing!
  end
  setmetatable(memory.unmapped, memory.unmapped.mt)
  memory.map_block(0, 0xFF, memory.unmapped)

  -- Main Memory
  memory.work_ram_0 = memory.generate_block(4 * 1024, 0xC000)
  memory.work_ram_1_raw = memory.generate_block(4 * 7 * 1024, 0xD000)
  memory.work_ram_1 = {}
  memory.work_ram_1.bank = 1
  memory.work_ram_1.mt = {}
  memory.work_ram_1.mt.__index = function(table, address)
    return memory.work_ram_1_raw[address + ((memory.work_ram_1.bank - 1) * 4 * 1024)]
  end
  memory.work_ram_1.mt.__newindex = function(table, address, value)
    memory.work_ram_1_raw[address + ((memory.work_ram_1.bank - 1) * 4 * 1024)] = value
  end
  setmetatable(memory.work_ram_1, memory.work_ram_1.mt)
  memory.map_block(0xC0, 0xCF, memory.work_ram_0, 0)
  memory.map_block(0xD0, 0xDF, memory.work_ram_1, 0)

  memory.work_ram_echo = {}
  memory.work_ram_echo.mt = {}
  memory.work_ram_echo.mt.__index = function(table, key)
    return memory.read_byte(key - 0xE000 + 0xC000)
  end
  memory.work_ram_echo.mt.__newindex = function(table, key, value)
    memory.write_byte(key - 0xE000 + 0xC000, value)
  end
  setmetatable(memory.work_ram_echo, memory.work_ram_echo.mt)
  memory.map_block(0xE0, 0xFD, memory.work_ram_echo, 0)

  memory.read_byte = function(address)
    local high_byte = bit32.band(address, 0xFF00)
    return block_map[high_byte][address]
  end

  memory.write_byte = function(address, byte)
    local high_byte = bit32.band(address, 0xFF00)
    block_map[high_byte][address] = byte
  end

  memory.reset = function()
    -- It's tempting to want to zero out all 0x0000-0xFFFF, but
    -- instead here we'll reset only that memory which this module
    -- DIRECTLY controls, so initialization logic can be performed
    -- elsewhere as appropriate.

    for i = 0xC000, 0xCFFF do
      memory.work_ram_0[i] = 0
    end

    for i = 0xD000, 0xDFFF do
      memory.work_ram_1[i] = 0
    end

    memory.work_ram_1.bank = 1
  end

  memory.save_state = function()
    local state = {}

    state.work_ram_0 = {}
    for i = 0xC000, 0xCFFF do
      state.work_ram_0[i] = memory.work_ram_0[i]
    end

    state.work_ram_1_raw = {}
    for i = 0xD000, (0xD000 + (4 * 7 * 1024) - 1) do
      state.work_ram_1_raw[i] = memory.work_ram_1_raw[i]
    end

    state.work_ram_1_bank = 1

    return state
  end

  memory.load_state = function(state)
    for i = 0xC000, 0xCFFF do
      memory.work_ram_0[i] = state.work_ram_0[i]
    end
    for i = 0xD000, (0xD000 + (4 * 7 * 1024) - 1) do
      memory.work_ram_1_raw[i] = state.work_ram_1_raw[i]
    end

    memory.work_ram_1.bank = state.work_ram_1_bank
  end

  -- Fancy: make access to ourselves act as an array, reading / writing memory using the above
  -- logic. This should cause memory[address] to behave just as it would on hardware.
  memory.mt = {}
  memory.mt.__index = function(table, key)
    return memory.read_byte(key)
  end
  memory.mt.__newindex = function(table, key, value)
    memory.write_byte(key, value)
  end
  setmetatable(memory, memory.mt)

  return memory
end

return Memory
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.rom_header" ] = function( ... ) local arg = _G.arg;
local bit32 = require("vendor.bitop-lua").bit32

local rom_header = {}
-- given an entire rom (as a string reference),
-- print out the various header data for debugging

local function read_file_into_byte_array(file)
    local byte_array = {}
    local byte = file:read()
    local i = 0
    while byte do
        byte_array[i] = byte
        byte = file:read()
        i = i + 1
    end
    return byte_array
end

local function extract_string(data, s, e)
    local str = ""
    for i = s, e do
        if data[i] ~= 0 then
            str = str .. string.char(data[i])
        end
    end
    return str
end

rom_header.mbc_names = {}
rom_header.mbc_names[0x00] = "ROM ONLY"
rom_header.mbc_names[0x01] = "MBC1"
rom_header.mbc_names[0x02] = "MBC1+RAM"
rom_header.mbc_names[0x03] = "MBC1+RAM+BATTERY"
rom_header.mbc_names[0x05] = "MBC2"
rom_header.mbc_names[0x06] = "MBC2+BATTERY"

rom_header.parse_cartridge_header = function(data)
    local header = {}
    --convert the title data into a lua string
    header.title = extract_string(data, 0x134, 0x143)
    header.manufacturer = extract_string(data, 0x13F, 0x142)

    local cgb = (bit32.band(data[0x143], 0x80) ~= 0)
    if cgb then
        header.color = true
        header.title = extract_string(data, 0x134, 0x13E)
    else
        header.color = false
    end

    header.licencee = extract_string(data, 0x144, 0x145)

    local sgb = data[0x146] == 0x3
    if sgb then
        header.super_gameboy = true
    else
        header.super_gameboy = false
    end

    header.mbc_type = data[0x147]
    header.mbc_name = rom_header.mbc_names[header.mbc_type]

    local rom_size = data[0x148]
    if rom_size < 0x8 then
        header.rom_size = bit32.lshift(32 * 1024, rom_size)
    end

    local ram_size = data[0x149]
    if ram_size == 0 then
        header.ram_size = 0
    end
    if ram_size == 1 then
        header.ram_size = 2 * 1024
    end
    if ram_size == 2 then
        header.ram_size = 8 * 1024
    end
    if ram_size == 3 then
        header.ram_size = 32 * 1024
    end

    local japanese = data[0x14A]
    if japanese then
        header.japanese = false
    else
        header.japanese = true
    end

    header.licensee_code = data[0x14B]

    return header
end

rom_header.print_cartridge_header = function(header)
    print("Title: ", header.title)
    print("Manufacturer: ", header.manufacturer)

    if header.color then
        print("Color: YES")
    else
        print("Color: NO")
    end

    print("Licencee: ", header.licencee)

    if header.super_gameboy then
        print("SuperGB: YES")
    else
        print("SuperGB: NO")
    end

    if rom_header.mbc_names[header.mbc_type] then
        print("MBC Type: " .. rom_header.mbc_names[header.mbc_type])
    else
        print("MBC Type: UNKNOWN: ", string.format("0x%02X", header.mbc_type))
    end

    print("ROM Size: ", header.rom_size)
    print("RAM Size: ", header.ram_size)

    if header.japanese then
        print("Japanese: Hai")
    else
        print("Japanese: Iie")
    end

    print("Licensee Code: ", header.licensee_code)
end

return rom_header
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.timers" ] = function( ... ) local arg = _G.arg;
 local bit32 = require("vendor.bitop-lua").bit32

local Timers = {}

function Timers.new(modules)
  local io = modules.io
  local interrupts = modules.interrupts

  local timers = {}

  local system_clocks_per_second = 4194304

  timers.system_clock = 0

  timers.clock_rates = {}

  function timers:set_normal_speed()
    self.clock_rates[0] = math.floor(system_clocks_per_second / 4096)
    self.clock_rates[1] = math.floor(system_clocks_per_second / 262144)
    self.clock_rates[2] = math.floor(system_clocks_per_second / 65536)
    self.clock_rates[3] = math.floor(system_clocks_per_second / 16384)
  end

  function timers:set_double_speed()
    self.clock_rates[0] = math.floor(system_clocks_per_second / 4096 / 2)
    self.clock_rates[1] = math.floor(system_clocks_per_second / 262144 / 2)
    self.clock_rates[2] = math.floor(system_clocks_per_second / 65536 / 2)
    self.clock_rates[3] = math.floor(system_clocks_per_second / 16384 / 2)
  end

  timers.div_base = 0
  timers.timer_offset = 0
  timers.timer_enabled = false

  io.write_logic[io.ports.DIV] = function(byte)
    -- Reset the DIV timer, in this case by re-basing it to the
    -- current system clock, which will roll it back to 0 on this cycle
    div_base = timers.system_clock
  end

  io.read_logic[io.ports.DIV] = function()
    return bit32.band(bit32.rshift(timers.system_clock - timers.div_base, 8), 0xFF)
  end

  io.write_logic[io.ports.TAC] = function(byte)
    io.ram[io.ports.TAC] = byte
    timers.timer_enabled = (bit32.band(io.ram[io.ports.TAC], 0x4) == 0x4)
    timers.timer_offset = timers.system_clock
  end

  function timers:update()
    if self.timer_enabled then
      local rate_select = bit32.band(io.ram[io.ports.TAC], 0x3)
      while self.system_clock > self.timer_offset + self.clock_rates[rate_select] do
        io.ram[io.ports.TIMA] = bit32.band(io.ram[io.ports.TIMA] + 1, 0xFF)
        self.timer_offset = self.timer_offset + self.clock_rates[rate_select]
        if io.ram[io.ports.TIMA] == 0x00 then
          --overflow happened, first reset TIMA to TMA
          io.ram[io.ports.TIMA] = io.ram[io.ports.TMA]
          --then, fire off the timer interrupt
          interrupts.raise(interrupts.Timer)
        end
      end
    end
  end

  function timers:reset()
    self.system_clock = 0
    self.div_base = 0
    self.timer_offset = 0
    self.timer_enabled = false
  end

  function timers:save_state()
    return {
      system_clock = self.system_clock,
      div_base = self.div_base,
      timer_offset = self.timer_offset,
      timer_enabled = self.timer_enabled}
  end

  function timers:load_state(state)
    self.system_clock = state.system_clock
    self.div_base = state.div_base
    self.timer_offset = state.timer_offset
    self.timer_enabled = state.timer_enabled
  end

  return timers
end

return Timers
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.z80" ] = function( ... ) local arg = _G.arg;
local bit32 = require("vendor.bitop-lua").bit32

local lshift = bit32.lshift
local rshift = bit32.rshift
local band = bit32.band
local bxor = bit32.bxor
local bor = bit32.bor
local bnot = bit32.bnot

local apply_arithmetic = require("gameboy.z80.arithmetic")
local apply_bitwise = require("gameboy.z80.bitwise")
local apply_call = require("gameboy.z80.call")
local apply_cp = require("gameboy.z80.cp")
local apply_inc_dec = require("gameboy.z80.inc_dec")
local apply_jp = require("gameboy.z80.jp")
local apply_ld = require("gameboy.z80.ld")
local apply_rl_rr_cb = require("gameboy.z80.rl_rr_cb")
local apply_stack = require("gameboy.z80.stack")

local Registers = require("gameboy.z80.registers")

local Z80 = {}

function Z80.new(modules)
    local z80 = {}

    local interrupts = modules.interrupts
    local io = modules.io
    local memory = modules.memory
    local timers = modules.timers

    -- local references, for shorter code
    local read_byte = memory.read_byte
    local write_byte = memory.write_byte

    z80.registers = Registers.new()
    local reg = z80.registers
    local flags = reg.flags

    -- Intentionally bad naming convention: I am NOT typing "registers"
    -- a bazillion times. The exported symbol uses the full name as a
    -- reasonable compromise.
    z80.halted = 0

    local add_cycles_normal = function(cycles)
        timers.system_clock = timers.system_clock + cycles
    end

    local add_cycles_double = function(cycles)
        timers.system_clock = timers.system_clock + cycles / 2
    end

    z80.add_cycles = add_cycles_normal

    z80.double_speed = false

    z80.reset = function(gameboy)
        -- Initialize registers to what the GB's
        -- iternal state would be after executing
        -- BIOS code

        flags.z = true
        flags.n = false
        flags.h = true
        flags.c = true

        if gameboy.type == gameboy.types.color then
            reg.a = 0x11
        else
            reg.a = 0x01
        end

        reg.b = 0x00
        reg.c = 0x13
        reg.d = 0x00
        reg.e = 0xD8
        reg.h = 0x01
        reg.l = 0x4D
        reg.pc = 0x100 --entrypoint for GB games
        reg.sp = 0xFFFE

        z80.halted = 0

        z80.double_speed = false
        z80.add_cycles = add_cycles_normal
        timers:set_normal_speed()
    end

    z80.save_state = function()
        local state = {}
        state.double_speed = z80.double_speed
        state.registers = z80.registers
        state.halted = z80.halted
        return state
    end

    z80.load_state = function(state)
        -- Note: doing this explicitly for safety, so as
        -- not to replace the table with external, possibly old / wrong structure
        flags.z = state.registers.flags.z
        flags.n = state.registers.flags.n
        flags.h = state.registers.flags.h
        flags.c = state.registers.flags.c

        z80.registers.a = state.registers.a
        z80.registers.b = state.registers.b
        z80.registers.c = state.registers.c
        z80.registers.d = state.registers.d
        z80.registers.e = state.registers.e
        z80.registers.h = state.registers.h
        z80.registers.l = state.registers.l
        z80.registers.pc = state.registers.pc
        z80.registers.sp = state.registers.sp

        z80.double_speed = state.double_speed
        if z80.double_speed then
            timers:set_double_speed()
        else
            timers:set_normal_speed()
        end
        z80.halted = state.halted
    end

    io.write_mask[0x4D] = 0x01

    local opcodes = {}
    local opcode_cycles = {}
    local opcode_names = {}

    -- Initialize the opcode_cycles table with 4 as a base cycle, so we only
    -- need to care about variations going forward
    for i = 0x00, 0xFF do
        opcode_cycles[i] = 4
    end

    function z80.read_at_hl()
        return memory.block_map[reg.h * 0x100][reg.h * 0x100 + reg.l]
    end

    function z80.set_at_hl(value)
        memory.block_map[reg.h * 0x100][reg.h * 0x100 + reg.l] = value
    end

    function z80.read_nn()
        local nn = read_byte(reg.pc)
        reg.pc = reg.pc + 1
        return nn
    end

    local read_at_hl = z80.read_at_hl
    local set_at_hl = z80.set_at_hl
    local read_nn = z80.read_nn

    apply_arithmetic(opcodes, opcode_cycles, z80, memory)
    apply_bitwise(opcodes, opcode_cycles, z80, memory)
    apply_call(opcodes, opcode_cycles, z80, memory, interrupts)
    apply_cp(opcodes, opcode_cycles, z80, memory)
    apply_inc_dec(opcodes, opcode_cycles, z80, memory)
    apply_jp(opcodes, opcode_cycles, z80, memory)
    apply_ld(opcodes, opcode_cycles, z80, memory)
    apply_rl_rr_cb(opcodes, opcode_cycles, z80, memory)
    apply_stack(opcodes, opcode_cycles, z80, memory)

    -- ====== GMB CPU-Controlcommands ======
    -- ccf
    opcodes[0x3F] = function()
        flags.c = not flags.c
        flags.n = false
        flags.h = false
    end

    -- scf
    opcodes[0x37] = function()
        flags.c = true
        flags.n = false
        flags.h = false
    end

    -- nop
    opcodes[0x00] = function() end

    -- halt
    opcodes[0x76] = function()
        --if interrupts_enabled == 1 then
        --print("Halting!")
        z80.halted = 1
        --else
        --print("Interrupts not enabled! Not actually halting...")
        --end
    end

    -- stop
    opcodes[0x10] = function()
        -- The stop opcode should always, for unknown reasons, be followed
        -- by an 0x00 data byte. If it isn't, this may be a sign that the
        -- emulator has run off the deep end, and this isn't a real STOP
        -- instruction.
        -- TODO: Research real hardware's behavior in these cases
        local stop_value = read_nn()
        if stop_value == 0x00 then
            print("STOP instruction not followed by NOP!")
            --halted = 1
        else
            print("Unimplemented WEIRDNESS after 0x10")
        end

        if band(io.ram[0x4D], 0x01) ~= 0 then
            --speed switch!
            print("Switching speeds!")
            if z80.double_speed then
                z80.add_cycles = add_cycles_normal
                z80.double_speed = false
                io.ram[0x4D] = band(io.ram[0x4D], 0x7E) + 0x00
                timers:set_normal_speed()
                print("Switched to Normal Speed")
            else
                z80.add_cycles = add_cycles_double
                z80.double_speed = true
                io.ram[0x4D] = band(io.ram[0x4D], 0x7E) + 0x80
                timers:set_double_speed()
                print("Switched to Double Speed")
            end
        end
    end

    -- di
    opcodes[0xF3] = function()
        interrupts.disable()
        --print("Disabled interrupts with DI")
    end
    -- ei
    opcodes[0xFB] = function()
        interrupts.enable()
        --print("Enabled interrupts with EI")
        z80.service_interrupt()
    end

    z80.service_interrupt = function()
        local fired = band(io.ram[0xFF], io.ram[0x0F])
        if fired ~= 0 then
            z80.halted = 0
            if interrupts.enabled ~= 0 then
                -- First, disable interrupts to prevent nesting routines (unless the program explicitly re-enables them later)
                interrupts.disable()

                -- Now, figure out which interrupt this is, and call the corresponding
                -- interrupt vector
                local vector = 0x40
                local count = 0
                while band(fired, 0x1) == 0 and count < 5 do
                    vector = vector + 0x08
                    fired = rshift(fired, 1)
                    count = count + 1
                end
                -- we need to clear the corresponding bit first, to avoid infinite loops
                io.ram[0x0F] = bxor(lshift(0x1, count), io.ram[0x0F])

                reg.sp = band(0xFFFF, reg.sp - 1)
                write_byte(reg.sp, rshift(band(reg.pc, 0xFF00), 8))
                reg.sp = band(0xFFFF, reg.sp - 1)
                write_byte(reg.sp, band(reg.pc, 0xFF))

                reg.pc = vector

                z80.add_cycles(12)
                return true
            end
        end
        return false
    end

    -- register this as a callback with the interrupts module
    interrupts.service_handler = z80.service_interrupt

    -- For any opcodes that at this point are undefined,
    -- go ahead and "define" them with the following panic
    -- function
    local function undefined_opcode()
        local opcode = read_byte(band(reg.pc - 1, 0xFFFF))
        print(string.format("Unhandled opcode!: %x", opcode))
    end

    for i = 0, 0xFF do
        if not opcodes[i] then
            opcodes[i] = undefined_opcode
        end
    end

    z80.process_instruction = function()
        --  If the processor is currently halted, then do nothing.
        if z80.halted == 0 then
            local opcode = read_byte(reg.pc)
            -- Advance to one byte beyond the opcode
            reg.pc = band(reg.pc + 1, 0xFFFF)
            -- Run the instruction
            opcodes[opcode]()

            -- add a base clock of 4 to every instruction
            -- NOPE, working on removing add_cycles, pull from the opcode_cycles
            -- table instead
            z80.add_cycles(opcode_cycles[opcode])
        else
            -- Base cycles of 4 when halted, for sanity
            z80.add_cycles(4)
        end

        return true
    end

    return z80
end

return Z80
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.z80.arithmetic" ] = function( ... ) local arg = _G.arg;
local bit32 = require("vendor.bitop-lua").bit32

local lshift = bit32.lshift
local rshift = bit32.rshift
local band = bit32.band
local bxor = bit32.bxor
local bor = bit32.bor
local bnor = bit32.bnor

function apply(opcodes, opcode_cycles, z80, memory)
    local read_at_hl = z80.read_at_hl
    local set_at_hl = z80.set_at_hl
    local read_nn = z80.read_nn
    local reg = z80.registers
    local flags = reg.flags

    local read_byte = memory.read_byte
    local write_byte = memory.write_byte

    local add_to_a = function(value)
        -- half-carry
        flags.h = band(reg.a, 0xF) + band(value, 0xF) > 0xF

        local sum = reg.a + value

        -- carry (and overflow correction)
        flags.c = sum > 0xFF

        reg.a = band(sum, 0xFF)

        flags.z = reg.a == 0
        flags.n = false
    end

    local adc_to_a = function(value)
        -- half-carry
        local carry = 0
        if flags.c then
            carry = 1
        end
        flags.h = band(reg.a, 0xF) + band(value, 0xF) + carry > 0xF
        local sum = reg.a + value + carry

        -- carry (and overflow correction)
        flags.c = sum > 0xFF
        reg.a = band(sum, 0xFF)

        flags.z = reg.a == 0
        flags.n = false
    end

    -- add A, r
    opcodes[0x80] = function() add_to_a(reg.b) end
    opcodes[0x81] = function() add_to_a(reg.c) end
    opcodes[0x82] = function() add_to_a(reg.d) end
    opcodes[0x83] = function() add_to_a(reg.e) end
    opcodes[0x84] = function() add_to_a(reg.h) end
    opcodes[0x85] = function() add_to_a(reg.l) end
    opcode_cycles[0x86] = 8
    opcodes[0x86] = function() add_to_a(read_at_hl()) end
    opcodes[0x87] = function() add_to_a(reg.a) end

    -- add A, nn
    opcode_cycles[0xC6] = 8
    opcodes[0xC6] = function() add_to_a(read_nn()) end

    -- adc A, r
    opcodes[0x88] = function() adc_to_a(reg.b) end
    opcodes[0x89] = function() adc_to_a(reg.c) end
    opcodes[0x8A] = function() adc_to_a(reg.d) end
    opcodes[0x8B] = function() adc_to_a(reg.e) end
    opcodes[0x8C] = function() adc_to_a(reg.h) end
    opcodes[0x8D] = function() adc_to_a(reg.l) end
    opcode_cycles[0x8E] = 8
    opcodes[0x8E] = function() adc_to_a(read_at_hl()) end
    opcodes[0x8F] = function() adc_to_a(reg.a) end

    -- adc A, nn
    opcode_cycles[0xCE] = 8
    opcodes[0xCE] = function() adc_to_a(read_nn()) end

    sub_from_a = function(value)
        -- half-carry
        flags.h = band(reg.a, 0xF) - band(value, 0xF) < 0
        reg.a = reg.a - value

        -- carry (and overflow correction)
        flags.c = reg.a < 0 or reg.a > 0xFF
        reg.a = band(reg.a, 0xFF)

        flags.z = reg.a == 0
        flags.n = true
    end

    sbc_from_a = function(value)
        local carry = 0
        if flags.c then
            carry = 1
        end
        -- half-carry
        flags.h = band(reg.a, 0xF) - band(value, 0xF) - carry < 0

        local difference = reg.a - value - carry

        -- carry (and overflow correction)
        flags.c = difference < 0 or difference > 0xFF
        reg.a = band(difference, 0xFF)

        flags.z = reg.a == 0
        flags.n = true
    end

    -- sub A, r
    opcodes[0x90] = function() sub_from_a(reg.b) end
    opcodes[0x91] = function() sub_from_a(reg.c) end
    opcodes[0x92] = function() sub_from_a(reg.d) end
    opcodes[0x93] = function() sub_from_a(reg.e) end
    opcodes[0x94] = function() sub_from_a(reg.h) end
    opcodes[0x95] = function() sub_from_a(reg.l) end
    opcode_cycles[0x96] = 8
    opcodes[0x96] = function() sub_from_a(read_at_hl()) end
    opcodes[0x97] = function() sub_from_a(reg.a) end

    -- sub A, nn
    opcode_cycles[0xD6] = 8
    opcodes[0xD6] = function() sub_from_a(read_nn()) end

    -- sbc A, r
    opcodes[0x98] = function() sbc_from_a(reg.b) end
    opcodes[0x99] = function() sbc_from_a(reg.c) end
    opcodes[0x9A] = function() sbc_from_a(reg.d) end
    opcodes[0x9B] = function() sbc_from_a(reg.e) end
    opcodes[0x9C] = function() sbc_from_a(reg.h) end
    opcodes[0x9D] = function() sbc_from_a(reg.l) end
    opcode_cycles[0x9E] = 8
    opcodes[0x9E] = function() sbc_from_a(read_at_hl()) end
    opcodes[0x9F] = function() sbc_from_a(reg.a) end

    -- sbc A, nn
    opcode_cycles[0xDE] = 8
    opcodes[0xDE] = function() sbc_from_a(read_nn()) end

    -- daa
    -- BCD adjustment, correct implementation details located here:
    -- http://www.z80.info/z80syntx.htm#DAA
    opcodes[0x27] = function()
        local a = reg.a
        if not flags.n then
            -- Addition Mode, adjust BCD for previous addition-like instruction
            if band(0xF, a) > 0x9 or flags.h then
                a = a + 0x6
            end
            if a > 0x9F or flags.c then
                a = a + 0x60
            end
        else
            -- Subtraction mode! Adjust BCD for previous subtraction-like instruction
            if flags.h then
                a = band(a - 0x6, 0xFF)
            end
            if flags.c then
                a = a - 0x60
            end
        end
        -- Always reset H and Z
        flags.h = false
        flags.z = false

        -- If a is greater than 0xFF, set the carry flag
        if band(0x100, a) == 0x100 then
            flags.c = true
        end
        -- Note: Do NOT clear the carry flag otherwise. This is how hardware
        -- behaves, yes it's weird.

        reg.a = band(a, 0xFF)
        -- Update zero flag based on A's contents
        flags.z = reg.a == 0
    end

    add_to_hl = function(value)
        -- half carry
        flags.h = band(reg.hl(), 0xFFF) + band(value, 0xFFF) > 0xFFF
        local sum = reg.hl() + value

        -- carry
        flags.c = sum > 0xFFFF or sum < 0x0000
        reg.set_hl(band(sum, 0xFFFF))
        flags.n = false
    end

    -- add HL, rr
    opcode_cycles[0x09] = 8
    opcode_cycles[0x19] = 8
    opcode_cycles[0x29] = 8
    opcode_cycles[0x39] = 8
    opcodes[0x09] = function() add_to_hl(reg.bc()) end
    opcodes[0x19] = function() add_to_hl(reg.de()) end
    opcodes[0x29] = function() add_to_hl(reg.hl()) end
    opcodes[0x39] = function() add_to_hl(reg.sp) end

    -- inc rr
    opcode_cycles[0x03] = 8
    opcodes[0x03] = function()
        reg.set_bc(band(reg.bc() + 1, 0xFFFF))
    end

    opcode_cycles[0x13] = 8
    opcodes[0x13] = function()
        reg.set_de(band(reg.de() + 1, 0xFFFF))
    end

    opcode_cycles[0x23] = 8
    opcodes[0x23] = function()
        reg.set_hl(band(reg.hl() + 1, 0xFFFF))
    end

    opcode_cycles[0x33] = 8
    opcodes[0x33] = function()
        reg.sp = band(reg.sp + 1, 0xFFFF)
    end

    -- dec rr
    opcode_cycles[0x0B] = 8
    opcodes[0x0B] = function()
        reg.set_bc(band(reg.bc() - 1, 0xFFFF))
    end

    opcode_cycles[0x1B] = 8
    opcodes[0x1B] = function()
        reg.set_de(band(reg.de() - 1, 0xFFFF))
    end

    opcode_cycles[0x2B] = 8
    opcodes[0x2B] = function()
        reg.set_hl(band(reg.hl() - 1, 0xFFFF))
    end

    opcode_cycles[0x3B] = 8
    opcodes[0x3B] = function()
        reg.sp = band(reg.sp - 1, 0xFFFF)
    end

    -- add SP, dd
    opcode_cycles[0xE8] = 16
    opcodes[0xE8] = function()
        local offset = read_nn()
        -- offset comes in as unsigned 0-255, so convert it to signed -128 - 127
        if band(offset, 0x80) ~= 0 then
            offset = offset + 0xFF00
        end

        -- half carry
        --if band(reg.sp, 0xFFF) + offset > 0xFFF or band(reg.sp, 0xFFF) + offset < 0 then
        flags.h = band(reg.sp, 0xF) + band(offset, 0xF) > 0xF
        -- carry
        flags.c = band(reg.sp, 0xFF) + band(offset, 0xFF) > 0xFF

        reg.sp = reg.sp + offset
        reg.sp = band(reg.sp, 0xFFFF)

        flags.z = false
        flags.n = false
    end
end

return apply
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.z80.bitwise" ] = function( ... ) local arg = _G.arg;
 local bit32 = require("vendor.bitop-lua").bit32

local lshift = bit32.lshift
local band = bit32.band
local band = bit32.band
local bxor = bit32.bxor
local bor = bit32.bor
local bnor = bit32.bnor

function apply(opcodes, opcode_cycles, z80, memory)
  local read_at_hl = z80.read_at_hl
  local set_at_hl = z80.set_at_hl
  local read_nn = z80.read_nn
  local reg = z80.registers
  local flags = reg.flags

  local read_byte = memory.read_byte
  local write_byte = memory.write_byte

  and_a_with = function(value)
    reg.a = band(reg.a, value)
    flags.z = reg.a == 0
    flags.n = false
    flags.h = true
    flags.c = false
  end

  -- and A, r
  opcodes[0xA0] = function() and_a_with(reg.b) end
  opcodes[0xA1] = function() and_a_with(reg.c) end
  opcodes[0xA2] = function() and_a_with(reg.d) end
  opcodes[0xA3] = function() and_a_with(reg.e) end
  opcodes[0xA4] = function() and_a_with(reg.h) end
  opcodes[0xA5] = function() and_a_with(reg.l) end
  opcode_cycles[0xA6] = 8
  opcodes[0xA6] = function() and_a_with(read_at_hl()) end
  opcodes[0xA7] = function()
    --reg.a = band(reg.a, value)
    flags.z = reg.a == 0
    flags.n = false
    flags.h = true
    flags.c = false
  end

  -- and A, nn
  opcode_cycles[0xE6] = 8
  opcodes[0xE6] = function() and_a_with(read_nn()) end

  xor_a_with = function(value)
    reg.a = bxor(reg.a, value)
    flags.z = reg.a == 0
    flags.n = false
    flags.h = false
    flags.c = false
  end

  -- xor A, r
  opcodes[0xA8] = function() xor_a_with(reg.b) end
  opcodes[0xA9] = function() xor_a_with(reg.c) end
  opcodes[0xAA] = function() xor_a_with(reg.d) end
  opcodes[0xAB] = function() xor_a_with(reg.e) end
  opcodes[0xAC] = function() xor_a_with(reg.h) end
  opcodes[0xAD] = function() xor_a_with(reg.l) end
  opcode_cycles[0xAE] = 8
  opcodes[0xAE] = function() xor_a_with(read_at_hl()) end
  opcodes[0xAF] = function()
    reg.a = 0
    flags.z = true
    flags.n = false
    flags.h = false
    flags.c = false
  end

  -- xor A, nn
  opcode_cycles[0xEE] = 8
  opcodes[0xEE] = function() xor_a_with(read_nn()) end

  or_a_with = function(value)
    reg.a = bor(reg.a, value)
    flags.z = reg.a == 0
    flags.n = false
    flags.h = false
    flags.c = false
  end

  -- or A, r
  opcodes[0xB0] = function() or_a_with(reg.b) end
  opcodes[0xB1] = function() or_a_with(reg.c) end
  opcodes[0xB2] = function() or_a_with(reg.d) end
  opcodes[0xB3] = function() or_a_with(reg.e) end
  opcodes[0xB4] = function() or_a_with(reg.h) end
  opcodes[0xB5] = function() or_a_with(reg.l) end
  opcode_cycles[0xB6] = 8
  opcodes[0xB6] = function() or_a_with(read_at_hl()) end
  opcodes[0xB7] = function()
    flags.z = reg.a == 0
    flags.n = false
    flags.h = false
    flags.c = false
  end

  -- or A, nn
  opcode_cycles[0xF6] = 8
  opcodes[0xF6] = function() or_a_with(read_nn()) end

  -- cpl
  opcodes[0x2F] = function()
    reg.a = bxor(reg.a, 0xFF)
    flags.n = true
    flags.h = true
  end
end

return apply
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.z80.call" ] = function( ... ) local arg = _G.arg;
 local bit32 = require("vendor.bitop-lua").bit32

local lshift = bit32.lshift
local rshift = bit32.rshift
local band = bit32.band
local bxor = bit32.bxor
local bor = bit32.bor
local bnor = bit32.bnor

function apply(opcodes, opcode_cycles, z80, memory, interrupts)
  local read_nn = z80.read_nn
  local reg = z80.registers
  local flags = reg.flags

  local read_byte = memory.read_byte
  local write_byte = memory.write_byte

  local call_nnnn = function()
    local lower = read_nn()
    local upper = read_nn() * 256
    -- at this point, reg.pc points at the next instruction after the call,
    -- so store the current PC to the stack

    reg.sp = (reg.sp + 0xFFFF) % 0x10000
    write_byte(reg.sp, rshift(reg.pc, 8))
    reg.sp = (reg.sp + 0xFFFF) % 0x10000
    write_byte(reg.sp, reg.pc % 0x100)

    reg.pc = upper + lower
  end

  -- call nn
  opcode_cycles[0xCD] = 24
  opcodes[0xCD] = function()
    call_nnnn()
  end

  -- call nz, nnnn
  opcode_cycles[0xC4] = 12
  opcodes[0xC4] = function()
    if not flags.z then
      call_nnnn()
      z80.add_cycles(12)
    else
      reg.pc = reg.pc + 2
    end
  end

  -- call nc, nnnn
  opcode_cycles[0xD4] = 12
  opcodes[0xD4] = function()
    if not flags.c then
      call_nnnn()
      z80.add_cycles(12)
    else
      reg.pc = reg.pc + 2
    end
  end

  -- call z, nnnn
  opcode_cycles[0xCC] = 12
  opcodes[0xCC] = function()
    if flags.z then
      call_nnnn()
      z80.add_cycles(12)
    else
      reg.pc = reg.pc + 2
    end
  end

  -- call c, nnnn
  opcode_cycles[0xDC] = 12
  opcodes[0xDC] = function()
    if flags.c then
      call_nnnn()
      z80.add_cycles(12)
    else
      reg.pc = reg.pc + 2
    end
  end

  local ret = function()
    local lower = read_byte(reg.sp)
    reg.sp = band(0xFFFF, reg.sp + 1)
    local upper = lshift(read_byte(reg.sp), 8)
    reg.sp = band(0xFFFF, reg.sp + 1)
    reg.pc = upper + lower
    z80.add_cycles(12)
  end

  -- ret
  opcodes[0xC9] = function() ret() end

  -- ret nz
  opcode_cycles[0xC0] = 8
  opcodes[0xC0] = function()
    if not flags.z then
      ret()
    end
  end

  -- ret nc
  opcode_cycles[0xD0] = 8
  opcodes[0xD0] = function()
    if not flags.c then
      ret()
    end
  end

  -- ret z
  opcode_cycles[0xC8] = 8
  opcodes[0xC8] = function()
    if flags.z then
      ret()
    end
  end

  -- ret c
  opcode_cycles[0xD8] = 8
  opcodes[0xD8] = function()
    if flags.c then
      ret()
    end
  end

  -- reti
  opcodes[0xD9] = function()
    ret()
    interrupts.enable()
    z80.service_interrupt()
  end

  -- note: used only for the RST instructions below
  local function call_address(address)
    -- reg.pc points at the next instruction after the call,
    -- so store the current PC to the stack
    reg.sp = band(0xFFFF, reg.sp - 1)
    write_byte(reg.sp, rshift(band(reg.pc, 0xFF00), 8))
    reg.sp = band(0xFFFF, reg.sp - 1)
    write_byte(reg.sp, band(reg.pc, 0xFF))

    reg.pc = address
  end

  -- rst N
  opcode_cycles[0xC7] = 16
  opcodes[0xC7] = function() call_address(0x00) end

  opcode_cycles[0xCF] = 16
  opcodes[0xCF] = function() call_address(0x08) end

  opcode_cycles[0xD7] = 16
  opcodes[0xD7] = function() call_address(0x10) end

  opcode_cycles[0xDF] = 16
  opcodes[0xDF] = function() call_address(0x18) end

  opcode_cycles[0xE7] = 16
  opcodes[0xE7] = function() call_address(0x20) end

  opcode_cycles[0xEF] = 16
  opcodes[0xEF] = function() call_address(0x28) end

  opcode_cycles[0xF7] = 16
  opcodes[0xF7] = function() call_address(0x30) end

  opcode_cycles[0xFF] = 16
  opcodes[0xFF] = function() call_address(0x38) end
end

return apply
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.z80.cp" ] = function( ... ) local arg = _G.arg;
 local bit32 = require("vendor.bitop-lua").bit32

local lshift = bit32.lshift
local band = bit32.band

function apply(opcodes, opcode_cycles, z80, memory)
  local read_at_hl = z80.read_at_hl
  local set_at_hl = z80.set_at_hl
  local read_nn = z80.read_nn
  local reg = z80.registers
  local flags = reg.flags

  local read_byte = memory.read_byte
  local write_byte = memory.write_byte

  cp_with_a = function(value)
    -- half-carry
    flags.h = (reg.a % 0x10) - (value % 0x10) < 0

    local temp = reg.a - value

    -- carry (and overflow correction)
    flags.c = temp < 0 or temp > 0xFF
    temp  = (temp + 0x100) % 0x100

    flags.z = temp == 0
    flags.n = true
  end

  -- cp A, r
  opcodes[0xB8] = function() cp_with_a(reg.b) end
  opcodes[0xB9] = function() cp_with_a(reg.c) end
  opcodes[0xBA] = function() cp_with_a(reg.d) end
  opcodes[0xBB] = function() cp_with_a(reg.e) end
  opcodes[0xBC] = function() cp_with_a(reg.h) end
  opcodes[0xBD] = function() cp_with_a(reg.l) end
  opcode_cycles[0xBE] = 8
  opcodes[0xBE] = function() cp_with_a(read_at_hl()) end
  opcodes[0xBF] = function() cp_with_a(reg.a) end

  -- cp A, nn
  opcode_cycles[0xFE] = 8
  opcodes[0xFE] = function() cp_with_a(read_nn()) end
end

return apply
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.z80.inc_dec" ] = function( ... ) local arg = _G.arg;
 local bit32 = require("vendor.bitop-lua").bit32

local lshift = bit32.lshift
local band = bit32.band

function apply(opcodes, opcode_cycles, z80, memory)
  local reg = z80.registers
  local flags = reg.flags

  local read_byte = memory.read_byte
  local write_byte = memory.write_byte

  set_inc_flags = function(value)
    flags.z = value == 0
    flags.h = value % 0x10 == 0x0
    flags.n = false
  end

  set_dec_flags = function(value)
    flags.z = value == 0
    flags.h = value % 0x10 == 0xF
    flags.n = true
  end

  -- inc r
  opcodes[0x04] = function() reg.b = band(reg.b + 1, 0xFF); set_inc_flags(reg.b) end
  opcodes[0x0C] = function() reg.c = band(reg.c + 1, 0xFF); set_inc_flags(reg.c) end
  opcodes[0x14] = function() reg.d = band(reg.d + 1, 0xFF); set_inc_flags(reg.d) end
  opcodes[0x1C] = function() reg.e = band(reg.e + 1, 0xFF); set_inc_flags(reg.e) end
  opcodes[0x24] = function() reg.h = band(reg.h + 1, 0xFF); set_inc_flags(reg.h) end
  opcodes[0x2C] = function() reg.l = band(reg.l + 1, 0xFF); set_inc_flags(reg.l) end
  opcode_cycles[0x34] = 12
  opcodes[0x34] = function()
    write_byte(reg.hl(), band(read_byte(reg.hl()) + 1, 0xFF))
    set_inc_flags(read_byte(reg.hl()))
  end
  opcodes[0x3C] = function() reg.a = band(reg.a + 1, 0xFF); set_inc_flags(reg.a) end

  -- dec r
  opcodes[0x05] = function() reg.b = band(reg.b - 1, 0xFF); set_dec_flags(reg.b) end
  opcodes[0x0D] = function() reg.c = band(reg.c - 1, 0xFF); set_dec_flags(reg.c) end
  opcodes[0x15] = function() reg.d = band(reg.d - 1, 0xFF); set_dec_flags(reg.d) end
  opcodes[0x1D] = function() reg.e = band(reg.e - 1, 0xFF); set_dec_flags(reg.e) end
  opcodes[0x25] = function() reg.h = band(reg.h - 1, 0xFF); set_dec_flags(reg.h) end
  opcodes[0x2D] = function() reg.l = band(reg.l - 1, 0xFF); set_dec_flags(reg.l) end
  opcode_cycles[0x35] = 12
  opcodes[0x35] = function()
    write_byte(reg.hl(), band(read_byte(reg.hl()) - 1, 0xFF))
    set_dec_flags(read_byte(reg.hl()))
  end
  opcodes[0x3D] = function() reg.a = band(reg.a - 1, 0xFF); set_dec_flags(reg.a) end
end

return apply
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.z80.jp" ] = function( ... ) local arg = _G.arg;
 local bit32 = require("vendor.bitop-lua").bit32

local lshift = bit32.lshift
local rshift = bit32.rshift
local band = bit32.band
local bxor = bit32.bxor
local bor = bit32.bor
local bnor = bit32.bnor

function apply(opcodes, opcode_cycles, z80, memory)
  local read_nn = z80.read_nn
  local reg = z80.registers
  local flags = reg.flags

  local read_byte = memory.read_byte
  local write_byte = memory.write_byte

  -- ====== GMB Jumpcommands ======
  local jump_to_nnnn = function()
    local lower = read_nn()
    local upper = lshift(read_nn(), 8)
    reg.pc = upper + lower
  end

  -- jp nnnn
  opcode_cycles[0xC3] = 16
  opcodes[0xC3] = function()
    jump_to_nnnn()
  end

  -- jp HL
  opcodes[0xE9] = function()
    reg.pc = reg.hl()
  end

  -- jp nz, nnnn
  opcode_cycles[0xC2] = 16
  opcodes[0xC2] = function()
    if not flags.z then
      jump_to_nnnn()
    else
      reg.pc = reg.pc + 2
      z80.add_cycles(-4)
    end
  end

  -- jp nc, nnnn
  opcode_cycles[0xD2] = 16
  opcodes[0xD2] = function()
    if not flags.c then
      jump_to_nnnn()
    else
      reg.pc = reg.pc + 2
      z80.add_cycles(-4)
    end
  end

  -- jp z, nnnn
  opcode_cycles[0xCA] = 16
  opcodes[0xCA] = function()
    if flags.z then
      jump_to_nnnn()
    else
      reg.pc = reg.pc + 2
      z80.add_cycles(-4)
    end
  end

  -- jp c, nnnn
  opcode_cycles[0xDA] = 16
  opcodes[0xDA] = function()
    if flags.c then
      jump_to_nnnn()
    else
      reg.pc = reg.pc + 2
      z80.add_cycles(-4)
    end
  end

  local function jump_relative_to_nn()
    local offset = read_nn()
    if offset > 127 then
      offset = offset - 256
    end
    reg.pc = (reg.pc + offset) % 0x10000
  end

  -- jr nn
  opcode_cycles[0x18] = 12
  opcodes[0x18] = function()
    jump_relative_to_nn()
  end

  -- jr nz, nn
  opcode_cycles[0x20] = 12
  opcodes[0x20] = function()
    if not flags.z then
      jump_relative_to_nn()
    else
      reg.pc = reg.pc + 1
      z80.add_cycles(-4)
    end
  end

  -- jr nc, nn
  opcode_cycles[0x30] = 12
  opcodes[0x30] = function()
    if not flags.c then
      jump_relative_to_nn()
    else
      reg.pc = reg.pc + 1
      z80.add_cycles(-4)
    end
  end

  -- jr z, nn
  opcode_cycles[0x28] = 12
  opcodes[0x28] = function()
    if flags.z then
      jump_relative_to_nn()
    else
      reg.pc = reg.pc + 1
      z80.add_cycles(-4)
    end
  end

  -- jr c, nn
  opcode_cycles[0x38] = 12
  opcodes[0x38] = function()
    if flags.c then
      jump_relative_to_nn()
    else
      reg.pc = reg.pc + 1
      z80.add_cycles(-4)
    end
  end
end

return apply
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.z80.ld" ] = function( ... ) local arg = _G.arg;
 local bit32 = require("vendor.bitop-lua").bit32

local lshift = bit32.lshift
local rshift = bit32.rshift
local band = bit32.band

function apply(opcodes, opcode_cycles, z80, memory)
  local read_at_hl = z80.read_at_hl
  local set_at_hl = z80.set_at_hl
  local read_nn = z80.read_nn
  local reg = z80.registers

  local read_byte = memory.read_byte
  local write_byte = memory.write_byte

  -- ld r, r
  opcodes[0x40] = function() reg.b = reg.b end
  opcodes[0x41] = function() reg.b = reg.c end
  opcodes[0x42] = function() reg.b = reg.d end
  opcodes[0x43] = function() reg.b = reg.e end
  opcodes[0x44] = function() reg.b = reg.h end
  opcodes[0x45] = function() reg.b = reg.l end
  opcode_cycles[0x46] = 8
  opcodes[0x46] = function() reg.b = read_at_hl() end
  opcodes[0x47] = function() reg.b = reg.a end

  opcodes[0x48] = function() reg.c = reg.b end
  opcodes[0x49] = function() reg.c = reg.c end
  opcodes[0x4A] = function() reg.c = reg.d end
  opcodes[0x4B] = function() reg.c = reg.e end
  opcodes[0x4C] = function() reg.c = reg.h end
  opcodes[0x4D] = function() reg.c = reg.l end
  opcode_cycles[0x4E] = 8
  opcodes[0x4E] = function() reg.c = read_at_hl() end
  opcodes[0x4F] = function() reg.c = reg.a end

  opcodes[0x50] = function() reg.d = reg.b end
  opcodes[0x51] = function() reg.d = reg.c end
  opcodes[0x52] = function() reg.d = reg.d end
  opcodes[0x53] = function() reg.d = reg.e end
  opcodes[0x54] = function() reg.d = reg.h end
  opcodes[0x55] = function() reg.d = reg.l end
  opcode_cycles[0x56] = 8
  opcodes[0x56] = function() reg.d = read_at_hl() end
  opcodes[0x57] = function() reg.d = reg.a end

  opcodes[0x58] = function() reg.e = reg.b end
  opcodes[0x59] = function() reg.e = reg.c end
  opcodes[0x5A] = function() reg.e = reg.d end
  opcodes[0x5B] = function() reg.e = reg.e end
  opcodes[0x5C] = function() reg.e = reg.h end
  opcodes[0x5D] = function() reg.e = reg.l end
  opcode_cycles[0x5E] = 8
  opcodes[0x5E] = function() reg.e = read_at_hl() end
  opcodes[0x5F] = function() reg.e = reg.a end

  opcodes[0x60] = function() reg.h = reg.b end
  opcodes[0x61] = function() reg.h = reg.c end
  opcodes[0x62] = function() reg.h = reg.d end
  opcodes[0x63] = function() reg.h = reg.e end
  opcodes[0x64] = function() reg.h = reg.h end
  opcodes[0x65] = function() reg.h = reg.l end
  opcode_cycles[0x66] = 8
  opcodes[0x66] = function() reg.h = read_at_hl() end
  opcodes[0x67] = function() reg.h = reg.a end

  opcodes[0x68] = function() reg.l = reg.b end
  opcodes[0x69] = function() reg.l = reg.c end
  opcodes[0x6A] = function() reg.l = reg.d end
  opcodes[0x6B] = function() reg.l = reg.e end
  opcodes[0x6C] = function() reg.l = reg.h end
  opcodes[0x6D] = function() reg.l = reg.l end
  opcode_cycles[0x6E] = 8
  opcodes[0x6E] = function() reg.l = read_at_hl() end
  opcodes[0x6F] = function() reg.l = reg.a end

opcode_cycles[0x70] = 8
  opcodes[0x70] = function() set_at_hl(reg.b) end

  opcode_cycles[0x71] = 8
  opcodes[0x71] = function() set_at_hl(reg.c) end

  opcode_cycles[0x72] = 8
  opcodes[0x72] = function() set_at_hl(reg.d) end

  opcode_cycles[0x73] = 8
  opcodes[0x73] = function() set_at_hl(reg.e) end

  opcode_cycles[0x74] = 8
  opcodes[0x74] = function() set_at_hl(reg.h) end

  opcode_cycles[0x75] = 8
  opcodes[0x75] = function() set_at_hl(reg.l) end

  -- 0x76 is HALT, we implement that elsewhere

  opcode_cycles[0x77] = 8
  opcodes[0x77] = function() set_at_hl(reg.a) end

  opcodes[0x78] = function() reg.a = reg.b end
  opcodes[0x79] = function() reg.a = reg.c end
  opcodes[0x7A] = function() reg.a = reg.d end
  opcodes[0x7B] = function() reg.a = reg.e end
  opcodes[0x7C] = function() reg.a = reg.h end
  opcodes[0x7D] = function() reg.a = reg.l end
  opcode_cycles[0x7E] = 8
  opcodes[0x7E] = function() reg.a = read_at_hl() end
  opcodes[0x7F] = function() reg.a = reg.a end

  -- ld r, n
  opcode_cycles[0x06] = 8
  opcodes[0x06] = function() reg.b = read_nn() end

  opcode_cycles[0x0E] = 8
  opcodes[0x0E] = function() reg.c = read_nn() end

  opcode_cycles[0x16] = 8
  opcodes[0x16] = function() reg.d = read_nn() end

  opcode_cycles[0x1E] = 8
  opcodes[0x1E] = function() reg.e = read_nn() end

  opcode_cycles[0x26] = 8
  opcodes[0x26] = function() reg.h = read_nn() end

  opcode_cycles[0x2E] = 8
  opcodes[0x2E] = function() reg.l = read_nn() end

  opcode_cycles[0x36] = 12
  opcodes[0x36] = function() set_at_hl(read_nn()) end

  opcode_cycles[0x3E] = 8
  opcodes[0x3E] = function() reg.a = read_nn() end

  -- ld A, (xx)
  opcode_cycles[0x0A] = 8
  opcodes[0x0A] = function()
    reg.a = read_byte(reg.bc())
  end

  opcode_cycles[0x1A] = 8
  opcodes[0x1A] = function()
    reg.a = read_byte(reg.de())
  end

  opcode_cycles[0xFA] = 16
  opcodes[0xFA] = function()
    local lower = read_nn()
    local upper = lshift(read_nn(), 8)
    reg.a = read_byte(upper + lower)
  end

  -- ld (xx), A
  opcode_cycles[0x02] = 8
  opcodes[0x02] = function()
    write_byte(reg.bc(), reg.a)
  end

  opcode_cycles[0x12] = 8
  opcodes[0x12] = function()
    write_byte(reg.de(), reg.a)
  end

  opcode_cycles[0xEA] = 16
  opcodes[0xEA] = function()
    local lower = read_nn()
    local upper = lshift(read_nn(), 8)
    write_byte(upper + lower, reg.a)
  end

  -- ld a, (FF00 + nn)
  opcode_cycles[0xF0] = 12
  opcodes[0xF0] = function()
    reg.a = read_byte(0xFF00 + read_nn())
  end

  -- ld (FF00 + nn), a
  opcode_cycles[0xE0] = 12
  opcodes[0xE0] = function()
    write_byte(0xFF00 + read_nn(), reg.a)
  end

  -- ld a, (FF00 + C)
  opcode_cycles[0xF2] = 8
  opcodes[0xF2] = function()
    reg.a = read_byte(0xFF00 + reg.c)
  end

  -- ld (FF00 + C), a
  opcode_cycles[0xE2] = 8
  opcodes[0xE2] = function()
    write_byte(0xFF00 + reg.c, reg.a)
  end

  -- ldi (HL), a
  opcode_cycles[0x22] = 8
  opcodes[0x22] = function()
    set_at_hl(reg.a)
    reg.set_hl(band(reg.hl() + 1, 0xFFFF))
  end

  -- ldi a, (HL)
  opcode_cycles[0x2A] = 8
  opcodes[0x2A] = function()
    reg.a = read_at_hl()
    reg.set_hl(band(reg.hl() + 1, 0xFFFF))
  end

  -- ldd (HL), a
  opcode_cycles[0x32] = 8
  opcodes[0x32] = function()
    set_at_hl(reg.a)
    reg.set_hl(band(reg.hl() - 1, 0xFFFF))
  end

  -- ldd a, (HL)
  opcode_cycles[0x3A] = 8
  opcodes[0x3A] = function()
    reg.a = read_at_hl()
    reg.set_hl(band(reg.hl() - 1, 0xFFFF))
  end

  -- ====== GMB 16-bit load commands ======
  -- ld BC, nnnn
  opcode_cycles[0x01] = 12
  opcodes[0x01] = function()
    reg.c = read_nn()
    reg.b = read_nn()
  end

  -- ld DE, nnnn
  opcode_cycles[0x11] = 12
  opcodes[0x11] = function()
    reg.e = read_nn()
    reg.d = read_nn()
  end

  -- ld HL, nnnn
  opcode_cycles[0x21] = 12
  opcodes[0x21] = function()
    reg.l = read_nn()
    reg.h = read_nn()
  end

  -- ld SP, nnnn
  opcode_cycles[0x31] = 12
  opcodes[0x31] = function()
    local lower = read_nn()
    local upper = lshift(read_nn(), 8)
    reg.sp = band(0xFFFF, upper + lower)
  end

  -- ld SP, HL
  opcode_cycles[0xF9] = 8
  opcodes[0xF9] = function()
    reg.sp = reg.hl()
  end

  -- ld HL, SP + dd
  opcode_cycles[0xF8] = 12
  opcodes[0xF8] = function()
    -- cheat
    local old_sp = reg.sp
    opcodes[0xE8]()
    reg.set_hl(reg.sp)
    reg.sp = old_sp
  end

  -- ====== GMB Special Purpose / Relocated Commands ======
  -- ld (nnnn), SP
  opcode_cycles[0x08] = 20
  opcodes[0x08] = function()
    local lower = read_nn()
    local upper = lshift(read_nn(), 8)
    local address = upper + lower
    write_byte(address, band(reg.sp, 0xFF))
    write_byte(band(address + 1, 0xFFFF), rshift(band(reg.sp, 0xFF00), 8))
  end
end

return apply
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.z80.registers" ] = function( ... ) local arg = _G.arg;
 local bit32 = require("vendor.bitop-lua").bit32

local lshift = bit32.lshift
local band = bit32.band
local rshift = bit32.rshift

local Registers = {}

function Registers.new()
  local registers = {}
  local reg = registers

  reg.a = 0
  reg.b = 0
  reg.c = 0
  reg.d = 0
  reg.e = 0
  reg.flags = {z=false,n=false,h=false,c=false}
  reg.h = 0
  reg.l = 0
  reg.pc = 0
  reg.sp = 0

  reg.f = function()
    local value = 0
    if reg.flags.z then
      value = value + 0x80
    end
    if reg.flags.n then
      value = value + 0x40
    end
    if reg.flags.h then
      value = value + 0x20
    end
    if reg.flags.c then
      value = value + 0x10
    end
    return value
  end

  reg.set_f = function(value)
    reg.flags.z = band(value, 0x80) ~= 0
    reg.flags.n = band(value, 0x40) ~= 0
    reg.flags.h = band(value, 0x20) ~= 0
    reg.flags.c = band(value, 0x10) ~= 0
  end

  reg.af = function()
    return lshift(reg.a, 8) + reg.f()
  end

  reg.bc = function()
    return lshift(reg.b, 8) + reg.c
  end

  reg.de = function()
    return lshift(reg.d, 8) + reg.e
  end

  reg.hl = function()
    return lshift(reg.h, 8) + reg.l
  end

  reg.set_bc = function(value)
    reg.b = rshift(band(value, 0xFF00), 8)
    reg.c = band(value, 0xFF)
  end

  reg.set_de = function(value)
    reg.d = rshift(band(value, 0xFF00), 8)
    reg.e = band(value, 0xFF)
  end

  reg.set_hl = function(value)
    reg.h = rshift(band(value, 0xFF00), 8)
    reg.l = band(value, 0xFF)
  end

  return registers
end

return Registers
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.z80.rl_rr_cb" ] = function( ... ) local arg = _G.arg;
 local bit32 = require("vendor.bitop-lua").bit32

local lshift = bit32.lshift
local rshift = bit32.rshift
local band = bit32.band
local bxor = bit32.bxor
local bor = bit32.bor
local bnor = bit32.bnor

function apply(opcodes, opcode_cycles, z80, memory)
  local read_nn = z80.read_nn
  local reg = z80.registers
  local flags = reg.flags

  local read_byte = memory.read_byte
  local write_byte = memory.write_byte

  local add_cycles = z80.add_cycles

  -- ====== GMB Rotate and Shift Commands ======
  local reg_rlc = function(value)
    value = lshift(value, 1)
    -- move what would be bit 8 into the carry
    flags.c = band(value, 0x100) ~= 0
    value = band(value, 0xFF)
    -- also copy the carry into bit 0
    if flags.c then
      value = value + 1
    end
    flags.z = value == 0
    flags.h = false
    flags.n = false
    return value
  end

  local reg_rl = function(value)
    value = lshift(value, 1)
    -- move the carry into bit 0
    if flags.c then
      value = value + 1
    end
    -- now move what would be bit 8 into the carry
    flags.c = band(value, 0x100) ~= 0
    value = band(value, 0xFF)

    flags.z = value == 0
    flags.h = false
    flags.n = false
    return value
  end

  local reg_rrc = function(value)
    -- move bit 0 into the carry
    flags.c = band(value, 0x1) ~= 0
    value = rshift(value, 1)
    -- also copy the carry into bit 7
    if flags.c then
      value = value + 0x80
    end
    flags.z = value == 0
    flags.h = false
    flags.n = false
    return value
  end

  local reg_rr = function(value)
    -- first, copy the carry into bit 8 (!!)
    if flags.c then
      value = value + 0x100
    end
    -- move bit 0 into the carry
    flags.c = band(value, 0x1) ~= 0
    value = rshift(value, 1)
    -- for safety, this should be a nop?
    -- value = band(value, 0xFF)
    flags.z = value == 0
    flags.h = false
    flags.n = false
    return value
  end

  -- rlc a
  opcodes[0x07] = function() reg.a = reg_rlc(reg.a); flags.z = false end

  -- rl a
  opcodes[0x17] = function() reg.a = reg_rl(reg.a); flags.z = false end

  -- rrc a
  opcodes[0x0F] = function() reg.a = reg_rrc(reg.a); flags.z = false end

  -- rr a
  opcodes[0x1F] = function() reg.a = reg_rr(reg.a); flags.z = false end

  -- ====== CB: Extended Rotate and Shift ======

  cb = {}

  -- rlc r
  cb[0x00] = function() reg.b = reg_rlc(reg.b); add_cycles(4) end
  cb[0x01] = function() reg.c = reg_rlc(reg.c); add_cycles(4) end
  cb[0x02] = function() reg.d = reg_rlc(reg.d); add_cycles(4) end
  cb[0x03] = function() reg.e = reg_rlc(reg.e); add_cycles(4) end
  cb[0x04] = function() reg.h = reg_rlc(reg.h); add_cycles(4) end
  cb[0x05] = function() reg.l = reg_rlc(reg.l); add_cycles(4) end
  cb[0x06] = function() write_byte(reg.hl(), reg_rlc(read_byte(reg.hl()))); add_cycles(12) end
  cb[0x07] = function() reg.a = reg_rlc(reg.a); add_cycles(4) end

  -- rl r
  cb[0x10] = function() reg.b = reg_rl(reg.b); add_cycles(4) end
  cb[0x11] = function() reg.c = reg_rl(reg.c); add_cycles(4) end
  cb[0x12] = function() reg.d = reg_rl(reg.d); add_cycles(4) end
  cb[0x13] = function() reg.e = reg_rl(reg.e); add_cycles(4) end
  cb[0x14] = function() reg.h = reg_rl(reg.h); add_cycles(4) end
  cb[0x15] = function() reg.l = reg_rl(reg.l); add_cycles(4) end
  cb[0x16] = function() write_byte(reg.hl(), reg_rl(read_byte(reg.hl()))); add_cycles(12) end
  cb[0x17] = function() reg.a = reg_rl(reg.a); add_cycles(4) end

  -- rrc r
  cb[0x08] = function() reg.b = reg_rrc(reg.b); add_cycles(4) end
  cb[0x09] = function() reg.c = reg_rrc(reg.c); add_cycles(4) end
  cb[0x0A] = function() reg.d = reg_rrc(reg.d); add_cycles(4) end
  cb[0x0B] = function() reg.e = reg_rrc(reg.e); add_cycles(4) end
  cb[0x0C] = function() reg.h = reg_rrc(reg.h); add_cycles(4) end
  cb[0x0D] = function() reg.l = reg_rrc(reg.l); add_cycles(4) end
  cb[0x0E] = function() write_byte(reg.hl(), reg_rrc(read_byte(reg.hl()))); add_cycles(12) end
  cb[0x0F] = function() reg.a = reg_rrc(reg.a); add_cycles(4) end

  -- rl r
  cb[0x18] = function() reg.b = reg_rr(reg.b); add_cycles(4) end
  cb[0x19] = function() reg.c = reg_rr(reg.c); add_cycles(4) end
  cb[0x1A] = function() reg.d = reg_rr(reg.d); add_cycles(4) end
  cb[0x1B] = function() reg.e = reg_rr(reg.e); add_cycles(4) end
  cb[0x1C] = function() reg.h = reg_rr(reg.h); add_cycles(4) end
  cb[0x1D] = function() reg.l = reg_rr(reg.l); add_cycles(4) end
  cb[0x1E] = function() write_byte(reg.hl(), reg_rr(read_byte(reg.hl()))); add_cycles(12) end
  cb[0x1F] = function() reg.a = reg_rr(reg.a); add_cycles(4) end

  local reg_sla = function(value)
    -- copy bit 7 into carry
    flags.c = band(value, 0x80) == 0x80
    value = band(lshift(value, 1), 0xFF)
    flags.z = value == 0
    flags.h = false
    flags.n = false
    add_cycles(4)
    return value
  end

  local reg_srl = function(value)
    -- copy bit 0 into carry
    flags.c = band(value, 0x1) == 1
    value = rshift(value, 1)
    flags.z = value == 0
    flags.h = false
    flags.n = false
    add_cycles(4)
    return value
  end

  local reg_sra = function(value)
    local arith_value = reg_srl(value)
    -- if bit 6 is set, copy it to bit 7
    if band(arith_value, 0x40) ~= 0 then
      arith_value = arith_value + 0x80
    end
    add_cycles(4)
    return arith_value
  end

  local reg_swap = function(value)
    value = rshift(band(value, 0xF0), 4) + lshift(band(value, 0xF), 4)
    flags.z = value == 0
    flags.n = false
    flags.h = false
    flags.c = false
    add_cycles(4)
    return value
  end

  -- sla r
  cb[0x20] = function() reg.b = reg_sla(reg.b) end
  cb[0x21] = function() reg.c = reg_sla(reg.c) end
  cb[0x22] = function() reg.d = reg_sla(reg.d) end
  cb[0x23] = function() reg.e = reg_sla(reg.e) end
  cb[0x24] = function() reg.h = reg_sla(reg.h) end
  cb[0x25] = function() reg.l = reg_sla(reg.l) end
  cb[0x26] = function() write_byte(reg.hl(), reg_sla(read_byte(reg.hl()))); add_cycles(8) end
  cb[0x27] = function() reg.a = reg_sla(reg.a) end

  -- swap r (high and low nybbles)
  cb[0x30] = function() reg.b = reg_swap(reg.b) end
  cb[0x31] = function() reg.c = reg_swap(reg.c) end
  cb[0x32] = function() reg.d = reg_swap(reg.d) end
  cb[0x33] = function() reg.e = reg_swap(reg.e) end
  cb[0x34] = function() reg.h = reg_swap(reg.h) end
  cb[0x35] = function() reg.l = reg_swap(reg.l) end
  cb[0x36] = function() write_byte(reg.hl(), reg_swap(read_byte(reg.hl()))); add_cycles(8) end
  cb[0x37] = function() reg.a = reg_swap(reg.a) end

  -- sra r
  cb[0x28] = function() reg.b = reg_sra(reg.b); add_cycles(-4) end
  cb[0x29] = function() reg.c = reg_sra(reg.c); add_cycles(-4) end
  cb[0x2A] = function() reg.d = reg_sra(reg.d); add_cycles(-4) end
  cb[0x2B] = function() reg.e = reg_sra(reg.e); add_cycles(-4) end
  cb[0x2C] = function() reg.h = reg_sra(reg.h); add_cycles(-4) end
  cb[0x2D] = function() reg.l = reg_sra(reg.l); add_cycles(-4) end
  cb[0x2E] = function() write_byte(reg.hl(), reg_sra(read_byte(reg.hl()))); add_cycles(4) end
  cb[0x2F] = function() reg.a = reg_sra(reg.a); add_cycles(-4) end

  -- srl r
  cb[0x38] = function() reg.b = reg_srl(reg.b) end
  cb[0x39] = function() reg.c = reg_srl(reg.c) end
  cb[0x3A] = function() reg.d = reg_srl(reg.d) end
  cb[0x3B] = function() reg.e = reg_srl(reg.e) end
  cb[0x3C] = function() reg.h = reg_srl(reg.h) end
  cb[0x3D] = function() reg.l = reg_srl(reg.l) end
  cb[0x3E] = function() write_byte(reg.hl(), reg_srl(read_byte(reg.hl()))); add_cycles(8) end
  cb[0x3F] = function() reg.a = reg_srl(reg.a) end

  -- ====== GMB Singlebit Operation Commands ======
  local reg_bit = function(value, bit)
    flags.z = band(value, lshift(0x1, bit)) == 0
    flags.n = false
    flags.h = true
    return
  end

  opcodes[0xCB] = function()
    local cb_op = read_nn()
    add_cycles(4)
    if cb[cb_op] ~= nil then
      --revert the timing; this is handled automatically by the various functions
      add_cycles(-4)
      cb[cb_op]()
      return
    end
    local high_half_nybble = rshift(band(cb_op, 0xC0), 6)
    local reg_index = band(cb_op, 0x7)
    local bit = rshift(band(cb_op, 0x38), 3)
    if high_half_nybble == 0x1 then
      -- bit n,r
      if reg_index == 0 then reg_bit(reg.b, bit) end
      if reg_index == 1 then reg_bit(reg.c, bit) end
      if reg_index == 2 then reg_bit(reg.d, bit) end
      if reg_index == 3 then reg_bit(reg.e, bit) end
      if reg_index == 4 then reg_bit(reg.h, bit) end
      if reg_index == 5 then reg_bit(reg.l, bit) end
      if reg_index == 6 then reg_bit(read_byte(reg.hl()), bit); add_cycles(4) end
      if reg_index == 7 then reg_bit(reg.a, bit) end
    end
    if high_half_nybble == 0x2 then
      -- res n, r
      -- note: this is REALLY stupid, but it works around some floating point
      -- limitations in Lua.
      if reg_index == 0 then reg.b = band(reg.b, bxor(reg.b, lshift(0x1, bit))) end
      if reg_index == 1 then reg.c = band(reg.c, bxor(reg.c, lshift(0x1, bit))) end
      if reg_index == 2 then reg.d = band(reg.d, bxor(reg.d, lshift(0x1, bit))) end
      if reg_index == 3 then reg.e = band(reg.e, bxor(reg.e, lshift(0x1, bit))) end
      if reg_index == 4 then reg.h = band(reg.h, bxor(reg.h, lshift(0x1, bit))) end
      if reg_index == 5 then reg.l = band(reg.l, bxor(reg.l, lshift(0x1, bit))) end
      if reg_index == 6 then write_byte(reg.hl(), band(read_byte(reg.hl()), bxor(read_byte(reg.hl()), lshift(0x1, bit)))); add_cycles(8) end
      if reg_index == 7 then reg.a = band(reg.a, bxor(reg.a, lshift(0x1, bit))) end
    end

    if high_half_nybble == 0x3 then
      -- set n, r
      if reg_index == 0 then reg.b = bor(lshift(0x1, bit), reg.b) end
      if reg_index == 1 then reg.c = bor(lshift(0x1, bit), reg.c) end
      if reg_index == 2 then reg.d = bor(lshift(0x1, bit), reg.d) end
      if reg_index == 3 then reg.e = bor(lshift(0x1, bit), reg.e) end
      if reg_index == 4 then reg.h = bor(lshift(0x1, bit), reg.h) end
      if reg_index == 5 then reg.l = bor(lshift(0x1, bit), reg.l) end
      if reg_index == 6 then write_byte(reg.hl(), bor(lshift(0x1, bit), read_byte(reg.hl()))); add_cycles(8) end
      if reg_index == 7 then reg.a = bor(lshift(0x1, bit), reg.a) end
    end
  end
end

return apply
end
end

do
local _ENV = _ENV
package.preload[ "gameboy.z80.stack" ] = function( ... ) local arg = _G.arg;
 local bit32 = require("vendor.bitop-lua").bit32

local band = bit32.band
local lshift = bit32.lshift
local rshift = bit32.rshift

function apply(opcodes, opcode_cycles, z80, memory)
  local reg = z80.registers

  local read_byte = memory.read_byte
  local write_byte = memory.write_byte

  -- push BC
  opcode_cycles[0xC5] = 16
  opcodes[0xC5] = function()
    reg.sp = band(0xFFFF, reg.sp - 1)
    write_byte(reg.sp, reg.b)
    reg.sp = band(0xFFFF, reg.sp - 1)
    write_byte(reg.sp, reg.c)
  end

  -- push DE
  opcode_cycles[0xD5] = 16
  opcodes[0xD5] = function()
    reg.sp = band(0xFFFF, reg.sp - 1)
    write_byte(reg.sp, reg.d)
    reg.sp = band(0xFFFF, reg.sp - 1)
    write_byte(reg.sp, reg.e)
  end

  -- push HL
  opcode_cycles[0xE5] = 16
  opcodes[0xE5] = function()
    reg.sp = band(0xFFFF, reg.sp - 1)
    write_byte(reg.sp, reg.h)
    reg.sp = band(0xFFFF, reg.sp - 1)
    write_byte(reg.sp, reg.l)
  end

  -- push AF
  opcode_cycles[0xF5] = 16
  opcodes[0xF5] = function()
    reg.sp = band(0xFFFF, reg.sp - 1)
    write_byte(reg.sp, reg.a)
    reg.sp = band(0xFFFF, reg.sp - 1)
    write_byte(reg.sp, reg.f())
  end

  -- pop BC
  opcode_cycles[0xC1] = 12
  opcodes[0xC1] = function()
    reg.c = read_byte(reg.sp)
    reg.sp = band(0xFFFF, reg.sp + 1)
    reg.b = read_byte(reg.sp)
    reg.sp = band(0xFFFF, reg.sp + 1)
  end

  -- pop DE
  opcode_cycles[0xD1] = 12
  opcodes[0xD1] = function()
    reg.e = read_byte(reg.sp)
    reg.sp = band(0xFFFF, reg.sp + 1)
    reg.d = read_byte(reg.sp)
    reg.sp = band(0xFFFF, reg.sp + 1)
  end

  -- pop HL
  opcode_cycles[0xE1] = 12
  opcodes[0xE1] = function()
    reg.l = read_byte(reg.sp)
    reg.sp = band(0xFFFF, reg.sp + 1)
    reg.h = read_byte(reg.sp)
    reg.sp = band(0xFFFF, reg.sp + 1)
  end

  -- pop AF
  opcode_cycles[0xF1] = 12
  opcodes[0xF1] = function()
    reg.set_f(read_byte(reg.sp))
    reg.sp = band(0xFFFF, reg.sp + 1)
    reg.a = read_byte(reg.sp)
    reg.sp = band(0xFFFF, reg.sp + 1)
  end
end

return apply
end
end

do
local _ENV = _ENV
package.preload[ "vendor.base64" ] = function( ... ) local arg = _G.arg;
--[[

 base64 -- v1.5.3 public domain Lua base64 encoder/decoder
 no warranty implied; use at your own risk

 Needs bit32.extract function. If not present it's implemented using BitOp
 or Lua 5.3 native bit operators. For Lua 5.1 fallbacks to pure Lua
 implementation inspired by Rici Lake's post:
   http://ricilake.blogspot.co.uk/2007/10/iterating-bits-in-lua.html

 author: Ilya Kolbin (iskolbin@gmail.com)
 url: github.com/iskolbin/lbase64

 COMPATIBILITY

 Lua 5.1+, LuaJIT

 LICENSE

 See end of file for license information.

--]]


local base64 = {}

_G.bit = require("vendor.bitop-lua").bit32

local extract = require("vendor.bitop-lua").extract -- Lua 5.2/Lua 5.3 in compatibility mode
if not extract then
    if _G.bit then                                  -- LuaJIT
        local shl, shr, band = _G.bit.lshift, _G.bit.rshift, _G.bit.band
        extract = function(v, from, width)
            return band(shr(v, from), shl(1, width) - 1)
        end
    elseif _G._VERSION == "Lua 5.1" then
        extract = function(v, from, width)
            local w = 0
            local flag = 2 ^ from
            for i = 0, width - 1 do
                local flag2 = flag + flag
                if v % flag2 >= flag then
                    w = w + 2 ^ i
                end
                flag = flag2
            end
            return w
        end
    else -- Lua 5.3+
        extract = load [[return function( v, from, width )
			return ( v >> from ) & ((1 << width) - 1)
		end]]()
    end
end


function base64.makeencoder(s62, s63, spad)
    local encoder = {}
    for b64code, char in pairs { [0] = 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
        'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y',
        'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
        'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0', '1', '2',
        '3', '4', '5', '6', '7', '8', '9', s62 or '+', s63 or '/', spad or '=' } do
        encoder[b64code] = char:byte()
    end
    return encoder
end

function base64.makedecoder(s62, s63, spad)
    local decoder = {}
    for b64code, charcode in pairs(base64.makeencoder(s62, s63, spad)) do
        decoder[charcode] = b64code
    end
    return decoder
end

local DEFAULT_ENCODER = base64.makeencoder()
local DEFAULT_DECODER = base64.makedecoder()

local char, concat = string.char, table.concat

function base64.encode(str, encoder, usecaching)
    encoder = encoder or DEFAULT_ENCODER
    local t, k, n = {}, 1, #str
    local lastn = n % 3
    local cache = {}
    for i = 1, n - lastn, 3 do
        local a, b, c = str:byte(i, i + 2)
        local v = a * 0x10000 + b * 0x100 + c
        local s
        if usecaching then
            s = cache[v]
            if not s then
                s = char(encoder[extract(v, 18, 6)], encoder[extract(v, 12, 6)], encoder[extract(v, 6, 6)],
                    encoder[extract(v, 0, 6)])
                cache[v] = s
            end
        else
            s = char(encoder[extract(v, 18, 6)], encoder[extract(v, 12, 6)], encoder[extract(v, 6, 6)],
                encoder[extract(v, 0, 6)])
        end
        t[k] = s
        k = k + 1
    end
    if lastn == 2 then
        local a, b = str:byte(n - 1, n)
        local v = a * 0x10000 + b * 0x100
        t[k] = char(encoder[extract(v, 18, 6)], encoder[extract(v, 12, 6)], encoder[extract(v, 6, 6)], encoder[64])
    elseif lastn == 1 then
        local v = str:byte(n) * 0x10000
        t[k] = char(encoder[extract(v, 18, 6)], encoder[extract(v, 12, 6)], encoder[64], encoder[64])
    end
    return concat(t)
end

function base64.decode(b64, decoder, usecaching)
    decoder = decoder or DEFAULT_DECODER
    local pattern = '[^%w%+%/%=]'
    if decoder then
        local s62, s63
        for charcode, b64code in pairs(decoder) do
            if b64code == 62 then
                s62 = charcode
            elseif b64code == 63 then
                s63 = charcode
            end
        end
        pattern = ('[^%%w%%%s%%%s%%=]'):format(char(s62), char(s63))
    end
    b64 = b64:gsub(pattern, '')
    local cache = usecaching and {}
    local t, k = {}, 1
    local n = #b64
    local padding = b64:sub(-2) == '==' and 2 or b64:sub(-1) == '=' and 1 or 0
    for i = 1, padding > 0 and n - 4 or n, 4 do
        local a, b, c, d = b64:byte(i, i + 3)
        local s
        if usecaching then
            local v0 = a * 0x1000000 + b * 0x10000 + c * 0x100 + d
            s = cache[v0]
            if not s then
                local v = decoder[a] * 0x40000 + decoder[b] * 0x1000 + decoder[c] * 0x40 + decoder[d]
                s = char(extract(v, 16, 8), extract(v, 8, 8), extract(v, 0, 8))
                cache[v0] = s
            end
        else
            local v = decoder[a] * 0x40000 + decoder[b] * 0x1000 + decoder[c] * 0x40 + decoder[d]
            s = char(extract(v, 16, 8), extract(v, 8, 8), extract(v, 0, 8))
        end
        t[k] = s
        k = k + 1
    end
    if padding == 1 then
        local a, b, c = b64:byte(n - 3, n - 1)
        local v = decoder[a] * 0x40000 + decoder[b] * 0x1000 + decoder[c] * 0x40
        t[k] = char(extract(v, 16, 8), extract(v, 8, 8))
    elseif padding == 2 then
        local a, b = b64:byte(n - 3, n - 2)
        local v = decoder[a] * 0x40000 + decoder[b] * 0x1000
        t[k] = char(extract(v, 16, 8))
    end
    return concat(t)
end

return base64

--[[
------------------------------------------------------------------------------
This software is available under 2 licenses -- choose whichever you prefer.
------------------------------------------------------------------------------
ALTERNATIVE A - MIT License
Copyright (c) 2018 Ilya Kolbin
Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
------------------------------------------------------------------------------
ALTERNATIVE B - Public Domain (www.unlicense.org)
This is free and unencumbered software released into the public domain.
Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
software, either in source code form or as a compiled binary, for any purpose,
commercial or non-commercial, and by any means.
In jurisdictions that recognize copyright laws, the author or authors of this
software dedicate any and all copyright interest in the software to the public
domain. We make this dedication for the benefit of the public at large and to
the detriment of our heirs and successors. We intend this dedication to be an
overt act of relinquishment in perpetuity of all present and future rights to
this software under copyright law.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
------------------------------------------------------------------------------
--]]
end
end

do
local _ENV = _ENV
package.preload[ "vendor.bitop-lua" ] = function( ... ) local arg = _G.arg;
local M = { _TYPE = 'module', _NAME = 'bitop.funcs', _VERSION = '1.0-0' }

local floor = math.floor

local MOD = 2 ^ 32
local MODM = MOD - 1

local function memoize(f)
    local mt = {}
    local t = setmetatable({}, mt)

    function mt:__index(k)
        local v = f(k)
        t[k] = v
        return v
    end

    return t
end

local function make_bitop_uncached(t, m)
    local function bitop(a, b)
        local res, p = 0, 1
        while a ~= 0 and b ~= 0 do
            local am, bm = a % m, b % m
            res = res + t[am][bm] * p
            a = (a - am) / m
            b = (b - bm) / m
            p = p * m
        end
        res = res + (a + b) * p
        return res
    end
    return bitop
end

local function make_bitop(t)
    local op1 = make_bitop_uncached(t, 2 ^ 1)
    local op2 = memoize(function(a)
        return memoize(function(b)
            return op1(a, b)
        end)
    end)
    return make_bitop_uncached(op2, 2 ^ (t.n or 1))
end

-- ok? probably not if running on a 32-bit int Lua number type platform
function M.tobit(x)
    return x % 2 ^ 32
end

M.bxor = make_bitop { [0] = { [0] = 0, [1] = 1 }, [1] = { [0] = 1, [1] = 0 }, n = 4 }
local bxor = M.bxor

function M.bnot(a) return MODM - a end

local bnot = M.bnot

function M.band(a, b) return ((a + b) - bxor(a, b)) / 2 end

local band = M.band

function M.bor(a, b) return MODM - band(MODM - a, MODM - b) end

local bor = M.bor

local lshift, rshift      -- forward declare

function M.rshift(a, disp) -- Lua5.2 insipred
    if disp < 0 then return lshift(a, -disp) end
    return floor(a % 2 ^ 32 / 2 ^ disp)
end

rshift = M.rshift

function M.lshift(a, disp) -- Lua5.2 inspired
    if disp < 0 then return rshift(a, -disp) end
    return (a * 2 ^ disp) % 2 ^ 32
end

lshift = M.lshift

function M.tohex(x, n) -- BitOp style
    n = n or 8
    local up
    if n <= 0 then
        if n == 0 then return '' end
        up = true
        n = -n
    end
    x = band(x, 16 ^ n - 1)
    return ('%0' .. n .. (up and 'X' or 'x')):format(x)
end

local tohex = M.tohex

function M.extract(n, field, width) -- Lua5.2 inspired
    width = width or 1
    return band(rshift(n, field), 2 ^ width - 1)
end

local extract = M.extract

function M.replace(n, v, field, width) -- Lua5.2 inspired
    width = width or 1
    local mask1 = 2 ^ width - 1
    v = band(v, mask1) -- required by spec?
    local mask = bnot(lshift(mask1, field))
    return band(n, mask) + lshift(v, field)
end

local replace = M.replace

function M.bswap(x) -- BitOp style
    local a = band(x, 0xff); x = rshift(x, 8)
    local b = band(x, 0xff); x = rshift(x, 8)
    local c = band(x, 0xff); x = rshift(x, 8)
    local d = band(x, 0xff)
    return lshift(lshift(lshift(a, 8) + b, 8) + c, 8) + d
end

local bswap = M.bswap

function M.rrotate(x, disp) -- Lua5.2 inspired
    disp = disp % 32
    local low = band(x, 2 ^ disp - 1)
    return rshift(x, disp) + lshift(low, 32 - disp)
end

local rrotate = M.rrotate

function M.lrotate(x, disp) -- Lua5.2 inspired
    return rrotate(x, -disp)
end

local lrotate = M.lrotate

M.rol = M.lrotate -- LuaOp inspired
M.ror = M.rrotate -- LuaOp insipred


function M.arshift(x, disp) -- Lua5.2 inspired
    local z = rshift(x, disp)
    if x >= 0x80000000 then z = z + lshift(2 ^ disp - 1, 32 - disp) end
    return z
end

local arshift = M.arshift

function M.btest(x, y) -- Lua5.2 inspired
    return band(x, y) ~= 0
end

--
-- Start Lua 5.2 "bit32" compat section.
--

M.bit32 = {} -- Lua 5.2 'bit32' compatibility


local function bit32_bnot(x)
    return (-1 - x) % MOD
end
M.bit32.bnot = bit32_bnot

local function bit32_bxor(a, b, c, ...)
    local z
    if b then
        a = a % MOD
        b = b % MOD
        z = bxor(a, b)
        if c then
            z = bit32_bxor(z, c, ...)
        end
        return z
    elseif a then
        return a % MOD
    else
        return 0
    end
end
M.bit32.bxor = bit32_bxor

local function bit32_band(a, b, c, ...)
    local z
    if b then
        a = a % MOD
        b = b % MOD
        z = ((a + b) - bxor(a, b)) / 2
        if c then
            z = bit32_band(z, c, ...)
        end
        return z
    elseif a then
        return a % MOD
    else
        return MODM
    end
end
M.bit32.band = bit32_band

local function bit32_bor(a, b, c, ...)
    local z
    if b then
        a = a % MOD
        b = b % MOD
        z = MODM - band(MODM - a, MODM - b)
        if c then
            z = bit32_bor(z, c, ...)
        end
        return z
    elseif a then
        return a % MOD
    else
        return 0
    end
end
M.bit32.bor = bit32_bor

function M.bit32.btest(...)
    return bit32_band(...) ~= 0
end

function M.bit32.lrotate(x, disp)
    return lrotate(x % MOD, disp)
end

function M.bit32.rrotate(x, disp)
    return rrotate(x % MOD, disp)
end

function M.bit32.lshift(x, disp)
    if disp > 31 or disp < -31 then return 0 end
    return lshift(x % MOD, disp)
end

function M.bit32.rshift(x, disp)
    if disp > 31 or disp < -31 then return 0 end
    return rshift(x % MOD, disp)
end

function M.bit32.arshift(x, disp)
    x = x % MOD
    if disp >= 0 then
        if disp > 31 then
            return (x >= 0x80000000) and MODM or 0
        else
            local z = rshift(x, disp)
            if x >= 0x80000000 then z = z + lshift(2 ^ disp - 1, 32 - disp) end
            return z
        end
    else
        return lshift(x, -disp)
    end
end

function M.bit32.extract(x, field, ...)
    local width = ... or 1
    if field < 0 or field > 31 or width < 0 or field + width > 32 then error 'out of range' end
    x = x % MOD
    return extract(x, field, ...)
end

function M.bit32.replace(x, v, field, ...)
    local width = ... or 1
    if field < 0 or field > 31 or width < 0 or field + width > 32 then error 'out of range' end
    x = x % MOD
    v = v % MOD
    return replace(x, v, field, ...)
end

--
-- Start LuaBitOp "bit" compat section.
--

M.bit = {} -- LuaBitOp "bit" compatibility

function M.bit.tobit(x)
    x = x % MOD
    if x >= 0x80000000 then x = x - MOD end
    return x
end

local bit_tobit = M.bit.tobit

function M.bit.tohex(x, ...)
    return tohex(x % MOD, ...)
end

function M.bit.bnot(x)
    return bit_tobit(bnot(x % MOD))
end

local function bit_bor(a, b, c, ...)
    if c then
        return bit_bor(bit_bor(a, b), c, ...)
    elseif b then
        return bit_tobit(bor(a % MOD, b % MOD))
    else
        return bit_tobit(a)
    end
end
M.bit.bor = bit_bor

local function bit_band(a, b, c, ...)
    if c then
        return bit_band(bit_band(a, b), c, ...)
    elseif b then
        return bit_tobit(band(a % MOD, b % MOD))
    else
        return bit_tobit(a)
    end
end
M.bit.band = bit_band

local function bit_bxor(a, b, c, ...)
    if c then
        return bit_bxor(bit_bxor(a, b), c, ...)
    elseif b then
        return bit_tobit(bxor(a % MOD, b % MOD))
    else
        return bit_tobit(a)
    end
end
M.bit.bxor = bit_bxor

function M.bit.lshift(x, n)
    return bit_tobit(lshift(x % MOD, n % 32))
end

function M.bit.rshift(x, n)
    return bit_tobit(rshift(x % MOD, n % 32))
end

function M.bit.arshift(x, n)
    return bit_tobit(arshift(x % MOD, n % 32))
end

function M.bit.rol(x, n)
    return bit_tobit(lrotate(x % MOD, n % 32))
end

function M.bit.ror(x, n)
    return bit_tobit(rrotate(x % MOD, n % 32))
end

function M.bit.bswap(x)
    return bit_tobit(bswap(x % MOD))
end

return M
end
end

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
        screen_data = nil,
        last_timestamp = 0
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

function handlers.cron(payload, response)
    print("cron tick")
    local current_timestamp = payload.Message.Timestamp
    if Emulator.last_timestamp ~= 0 then
        local ticks = current_timestamp - Emulator.last_timestamp
        local calls = (ticks / 1000) * 60 / 10
        print("Calling it " .. tostring(calls))
        for _ = 1, calls do
            Emulator.gameboy:run_until_vblank()
        end
    end

    Emulator.last_timestamp = current_timestamp
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
actions.add("Cron", { function(payload, response) handlers.cron(payload, response) end })


pcall(require, "vendor.base64")
pcall(require, "vendor.bitop-lua")
