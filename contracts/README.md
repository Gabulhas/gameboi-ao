# Quantum AO

A monorepo containing all codebases related to Quantum and AO.

## Developer Guides

Below you will find the following guides:

- [Getting Started](#getting-started)
- [Compiling Processes](#compiling-processes)
- [Deploying Processes](#deploying-processes)

### Getting Started

1. Install Lua 5.3 using your preferred installation method. Some installation
   examples are below.

   - Follow the generic download instructions at
     https://www.lua.org/download.html

   - For macOS, you can use [Homebrew](https://brew.sh/)

     ```
     brew install lua@5.3
     ```

1. Install Node v20.x. You can follow download instructions
   [here](https://nodejs.org/en/download/package-manager).

1. Clone this repo.

   ```
   git clone git@github.com:astro-protocol/war-backend.git

   cd war-backend
   ```

1. Install `pnpm` package manager CLI.

   ```
   npm install -g pnpm
   ```

1. Install this project's dependencies.

   ```
   pnpm install
   ```

1. Create your `deploy.config.ts` file.

   ```
   cp deploy.config.example.ts deploy.config.ts
   ```

   > **IMPORTANT**
   >
   > Open your `deploy.config.ts` file and make sure your `luaPaths` paths are
   > correct for **your machine**. These paths will be used to create the
   > `LUA_PATH` that will be used to resolve `require` statements.
   >
   > If your paths are not correct, you may not be able to build/deploy this
   > project properly.

1. Create your `configs_qar_token.lua` and `configs_usda_token.lua` files.

   ```
   cp configs_qar_token_example.lua configs_qar_token.lua
   cp configs_usda_token_example.lua configs_usda_token.lua
   ```

   > **Note**
   >
   > The Lua files in the `src/processes/qar_token` directory reference this
   > file via `require` statements.

_End of steps_

### Compiling Processes

Use the following command to compile a process using your defined in your
`deploy.config.ts` file.

```bash
pnpm process compile <config-key>

# Example
#
# pnpm process compile qar_token
```

The `<config-key>` is any key defined in your `config` variable. For example, if
you have a `qar_token` key in your `config` variable like below ...

```ts
//
// ... code shortened for brevity
//

const config: Record<string, ProcessDeployConfigs> = {};

config.qar_token = {
  luaPath,
  wallet,
  name: "CRKST-001",
  contractPath: "./builds/qar_token.min.lua",
  //
  // ... code shortened for brevity
  //
};

export deafult config;
```

... then you can provide that key to the `pnpm process compile` command like
below:

```bash
pnpm process compile qar_token
```

When you compile code, two files will be created in the `./builds` directory:

- A non-minified `<config-key>.lua` file
  - This file is committed to source code to allow for easier code reviewing.
    This code is what should be deployed in mainnet environments so it is public
    and readable.

- A partially minified `<config-key>.min.lua` file
  - This file is not committed to source code and can be used in testnet
    environments so that it is not easily copied while undergoing active
    development.

### Deploying Processes

Use the following command to deploy a process using your defined in your
`deploy.config.ts` file.

```bash
pnpm process deploy <config-key> --run

# Example
#
# pnpm process deploy qar_token --run
```

The `<config-key>` is any key defined in your `config` variable. For example, if
you have a `qar_token` key in your `config` variable like below ...

```ts
//
// ... code shortened for brevity
//

const config: Record<string, ProcessDeployConfigs> = {};

config.qar_token = {
  luaPath,
  wallet,
  name: "CRKST-001",
  contractPath: "./builds/qar_token.min.lua",
  //
  // ... code shortened for brevity
  //
};

export deafult config;
```

... then you can provide that key to the `pnpm process compile` command like
below:

```bash
pnpm process deploy qar_token
```

The `contractPath` is the file that will be deployed. This value should be one
of the files in the `./builds` directory. For example,
`./builds/<config-key>.lua` or `./builds/<config-key>.min.lua`.
