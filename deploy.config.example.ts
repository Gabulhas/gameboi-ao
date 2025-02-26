import type { deployContract } from "ao-deploy";

type AoDeployConfigs = Parameters<typeof deployContract>[0];

type ProcessDeployConfigs = AoDeployConfigs & {
  compileOpts: {
    /** The source file to use to build the single contract file. */
    sourceFile: string;

    /** The file to create from the `compileOpts.source` file. */
    outputFile: string;
  };
};

const wallet = "./keys/aos.key.json";

// Lua modules (via require "some.module") will be resolved using this array
// starting from top to bottom.
//
// The first path that matches a module will be used to resolve that module.
//
const luaPaths = [
  "./node_modules/@astro-protocol/?.lua",
  "./node_modules/@astro-protocol/astropacks-aostandard/?.lua",
  "/usr/local/share/lua/5.3/?.lua",
  "./src/processes/?.lua",
  "../?.lua",
  "../../?.lua",
];

const luaPath = luaPaths.join(";");

const config: Record<string, ProcessDeployConfigs> = {};

// You can deploy using this config by doing:
//
//     $ pnpm process compile qar_token
//     $ pnpm process deploy qar_token --run
//
config.qar_token = {
  luaPath, // The list of paths to help resolve modules
  wallet, // The wallet to use when performing an AO deployment
  name: "CRKST-001", // The name to assign to the process. This is only used on the first deployment and cannot be changed. Changing it will deploy a new process.
  contractPath: "./builds/qar_token.min.lua", // The file to deloy to AO.
  module: "", // (Optional) An AO module ID. Deploying with a module ID will assign this process to that module. The latest module will be used if not set.
  scheduler: "_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA", // (Optional) An AO module ID. Deploying with a scheduler ID will assign this process to that scheduler. The latest scheduler will be used if not set.
  compileOpts: {
    sourceFile: "./src/processes/qar_token/process.lua", // The source code that should be compiled by the compiler
    outputFile: "./builds/qar_token.lua", // The output file to create. This will contain the compiled code.
  },
};

// You can deploy using this config by doing:
//
//     $ pnpm process compile qar_token_cron
//     $ pnpm process deploy qar_token_cron --run
//
config.qar_token_cron = {
  luaPath, // The list of paths to help resolve modules
  wallet, // The wallet to use when performing an AO deployment
  name: "CRKST-001 Message Processor", // The name to assign to the process. This is only used on the first deployment and cannot be changed. Changing it will deploy a new process.
  contractPath: "./builds/qar_token_cron.min.lua", // The file to deloy to AO.
  module: "", // (Optional) An AO module ID. Deploying with a module ID will assign this process to that module. The latest module will be used if not set.
  scheduler: "_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA", // (Optional) An AO module ID. Deploying with a scheduler ID will assign this process to that scheduler. The latest scheduler will be used if not set.
  cron: "1-minute", // The interval this process should tick at (e.g., 1-minute, 5-minutes).
  compileOpts: {
    sourceFile: "./src/processes/qar_token_cron/process.lua", // The source code that should be compiled by the compiler
    outputFile: "./builds/qar_token_cron.lua", // The output file to create. This will contain the compiled code.
  },
};

config.multisig = {
  luaPath, // The list of paths to help resolve modules
  wallet, // The wallet to use when performing an AO deployment
  name: "Quantum Bridge Multisig", // The name to assign to the process. This is only used on the first deployment and cannot be changed. Changing it will deploy a new process.
  contractPath: "./builds/multisig.min.lua", // The file to deloy to AO.
  module: "", // (Optional) An AO module ID. Deploying with a module ID will assign this process to that module. The latest module will be used if not set.
  scheduler: "_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA", // (Optional) An AO module ID. Deploying with a scheduler ID will assign this process to that scheduler. The latest scheduler will be used if not set.
  cron: "5-minute", // The interval this process should tick at (e.g., 1-minute, 5-minutes).
  compileOpts: {
    sourceFile: "./src/processes/multisig/process.lua", // The source code that should be compiled by the compiler
    outputFile: "./builds/multisig.lua", // The output file to create. This will contain the compiled code.
 },
};

// You can deploy using this config by doing:
//
//     $ pnpm process compile usda_token
//     $ pnpm process deploy usda_token --run
//
config.usda_token = {
  luaPath, // The list of paths to help resolve modules
  wallet, // The wallet to use when performing an AO deployment
  name: "CRKST-002", // The name to assign to the process. This is only used on the first deployment and cannot be changed. Changing it will deploy a new process.
  contractPath: "./builds/usda_token.min.lua", // The file to deloy to AO.
  module: "", // (Optional) An AO module ID. Deploying with a module ID will assign this process to that module. The latest module will be used if not set.
  scheduler: "_GQ33BkPtZrqxA84vM8Zk-N2aO0toNNu_C-l-rawrBA", // (Optional) An AO module ID. Deploying with a scheduler ID will assign this process to that scheduler. The latest scheduler will be used if not set.
  compileOpts: {
    sourceFile: "./src/processes/usda_token/process.lua", // The source code that should be compiled by the compiler
    outputFile: "./builds/usda_token.lua", // The output file to create. This will contain the compiled code.
  },
};

export default config;
