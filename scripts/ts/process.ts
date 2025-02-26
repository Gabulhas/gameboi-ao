import fs from "node:fs";
import path from "node:path";
import aoconnect from "@permaweb/aoconnect";

import LuaLoader from "./LuaLoader.ts";
import { deployContract } from "ao-deploy";
import config from "../../deploy.config.ts";

function toDoubleDigits(v: number) {
    if (v >= 10) {
        return `${v}`;
    }

    return `0${v}`;
}

function logUnknownConfigKey(configKey: string) {
    console.log(
        `Argument <config-key> '${configKey}' does not exist in process configs.`,
    );
    console.log(`\nDid you mean one of the following?`);
    console.log(`\n  - ${Object.keys(config).join("\n  - ")}\n`);
}

function validateConfigKey(command: string, configKey: string) {
    if (!configKey) {
        console.log(`Usage: process ${command} <config-key>`);
        console.log(`\nConfig Keys`);
        console.log(`\n  - ${Object.keys(config).join("\n  - ")}\n`);
        process.exit(1);
    }

    if (!config[configKey]) {
        logUnknownConfigKey(configKey);
        process.exit(1);
    }
}

class Commands {
    /**
     * Compile the given `.lua` file -- including all of its required modules.
     * @param configKey Any key in the `deploy.config.ts` file. For example, if
     * there is a `my_process` key on the exported `config` variable, then the
     * `my_process` key can be passed here to target its configs.
     */
    async compile(configKey: string, configFilePath?: string) {
        const processConfigs = config[configKey];

        const luaLoader = new LuaLoader(
            `${processConfigs.luaPath};${process.env.LUA_PATH}`,
        );

        let compiled = luaLoader.loadProcess(
            processConfigs.compileOpts.sourceFile,
        );
        console.log(`Compiled ${compiled.length}`);

        const now = new Date(Date.now());

        // compiled = compiled.replace(
        //   /\{version_app_build\}/g,
        //   `${now.getFullYear()}${
        //     toDoubleDigits(now.getMonth() + 1)
        //   }${now.getDate()}.${now.getHours()}${now.getMinutes()}${now.getSeconds()}`,
        // );

        const compiledProcessFile = path.join(
            process.cwd(),
            processConfigs.compileOpts.outputFile,
        );

        const compiledProcessFileMin = path.join(
            process.cwd(),
            processConfigs.compileOpts.outputFile.replace(".lua", ".min.lua"),
        );

        fs.writeFileSync(
            compiledProcessFile,
            compiled,
            { encoding: "utf-8" },
        );

        const { minify } = await import("./minifyUtils.ts");

        fs.writeFileSync(
            compiledProcessFileMin,
            compiled,
            { encoding: "utf-8" },
        );

        console.log(`\n\nFiles written to:`);
        console.log(`  - ${compiledProcessFile}`);
        console.log(`  - ${compiledProcessFileMin}`);
    }

    /**
     * Connect to a process.
     */
    connect(configKey: string) {
        const processConfigs = config[configKey];

        // TODO(crookse) Make cross-platform. This only works for *nix systems.
        process.stdout.write(
            `aos "${processConfigs.name}" --wallet=${processConfigs.wallet}`,
        );
    }

    /**
     * Deploy a process.
     *
     * @param configKey Any key in the `deploy.config.ts` file. For example, if
     * there is a `my_process` key on the exported `config` variable, then the
     * `my_process` key can be passed here to target its configs.
     */
    async deploy(configKey: string, configFilePath?: string) {
        try {
            const processConfigsAll = config[configKey];

            // Only get the ao-deploy related configs
            const { compileOpts, ...processConfigs } = processConfigsAll;

            await this.compile(configKey, configFilePath);

            console.log(`\nDeploying with the following configs:`);
            console.log(processConfigs);

            if (!process.argv.includes("--run")) {
                console.log(
                    `\n\nNo --run flag was provided. Exiting. To deploy, re-run this command with --run.\n`,
                );
                return;
            }

            const results = await deployContract(processConfigs);
            const { messageId, processId } = results;

            const processUrl = `https://www.ao.link/#/entity/${processId}`;
            const messageUrl = `https://www.ao.link/#/message/${messageId}`;

            console.log();
            console.log(`Process ID: ${processUrl}`);
            console.log(`Message ID: ${messageUrl}`);
            console.log();
        } catch (error) {
            console.log("\nDeployment failed!\n");
            console.log(error?.message ?? "Failed to deploy process!");

            throw error;
        }
    }
}

(async () => {
    const command = process.argv[2];

    console.log(command);

    if (!command || command === "") {
        console.log(`No command argument provided`);
        return;
    }

    const configKey = process.argv[3];
    let configFilePath;

    for (const value of process.argv) {
        const configFlag = "--config=";
        if (value.includes(configFlag)) {
            configFilePath = value
                .replace(configFlag, "") // Remove the prefix so we just get the path
                .replaceAll("/", ".") // Make the path conform to a Lua path syntax
                .replace(/\.lua$/, ""); // Remove the extension (if any)
        }
    }

    const commands = new Commands();

    if (command === "compile") {
        validateConfigKey(command, configKey);
        return commands.compile(configKey, configFilePath);
    }

    if (command === "connect") {
        validateConfigKey(command, configKey);
        return commands.connect(configKey);
    }

    if (command === "deploy") {
        validateConfigKey(command, configKey);
        await commands.deploy(configKey, configFilePath);

        return;
    }

    console.log(`Unknown command: ${command}\n`);
    process.exit(1);
})();
