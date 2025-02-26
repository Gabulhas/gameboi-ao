import fs from "fs";
import Arweave from "arweave";

function log(msg: string, ...rest: unknown[]) {
  console.log(`[Generate] ${msg}`, ...rest);
}

function logError(msg: string, ...rest: unknown[]) {
  console.log(`\n[Generate] [ERROR] ${msg}`, ...rest);
  console.log();
}

class Commands {
  /**
   * Create a `.json` file for the given private key.
   * @param dir The directory to store the key.
   * @param jwk The private key object.
   */
  createPrivateKeyJsonFile(dir: string, jwk: JsonWebKey) {
    const filepath = `${dir}/key.json`;

    log(`Checking if private key file already exists ...`);

    if (fs.existsSync(filepath)) {
      logError(`File "${filepath} already exists. Skipping creation.`);
      process.exit(1);
    }

    log(`... file does not exist. Creating private key file ...`);

    fs.writeFileSync(filepath, JSON.stringify(jwk), "utf-8");

    return filepath;
  }

  /**
   * Create a `.js` file for development purposes.
   * @param dir The directory to store the file.
   * @param jwk The private key object.
   * @param address The private key's public address.
   */
  createPrivateKeyDevJsFile(
    dir: string,
    jwk: JsonWebKey,
    address: string,
  ) {
    const filepath = `${dir}/key.js`;
    const jwkString = JSON.stringify(jwk);

    const jsFileContents = `
  const ADDRESS = "${address}";
  const KEY = ${jwkString};

  const key = {
    /** This key's address. */
    address: ADDRESS,

    /** Get the JSON represenation of this key. */
    json: () => KEY,

    /** Get the string representation of this key. */
    string: () => JSON.stringify(KEY),
  };

  export default key;
  `;
    log(`Creating private key JS file for development purposes ...`);

    fs.writeFileSync(filepath, jsFileContents, {
      encoding: "utf-8",
      "flag": "",
    });

    return filepath;
  }

  /**
   * Generate a private key JWK (`.json`) file and a corresponding `.js` file with
   * the private key's public address.
   * @param name The key file's name.
   */
  async generatePrivateKey(name: string) {
    try {
      if (!name) {
        throw new Error("Argument 'name' is required");
      }

      const arweave = new Arweave({});

      const dir = `./keys/${name}`;

      log(`Generating new "${dir}" private key ...`);

      const key = await arweave.wallets.generate();

      fs.mkdirSync(
        dir,
        { recursive: true },
      );

      log(`Getting private key address ...`);

      const address = await arweave.wallets.getAddress(key);

      const jsonFile = this.createPrivateKeyJsonFile(dir, key);

      if (!jsonFile) {
        return;
      }

      const jsFile = this.createPrivateKeyDevJsFile(dir, key, address);

      log(`Your "${dir}" private key files are located at:`);

      const keyFiles = [
        jsonFile,
        jsFile,
      ].join("\n  - ");

      console.log(`  - ${keyFiles}`);
    } catch (e) {
      logError(e.message);
      process.exit(1);
    }
  }
}

(async () => {
  const arg = process.argv[2];

  console.log();

  if (!arg || arg === "") {
    console.log(`No command argument provided`);
    return;
  }

  const commands = new Commands();

  if (arg === "private-key") {
    const keyName = process.argv[3];

    if (!keyName) {
      console.log(`Usage: generate private-key <name>\n\n`);
      process.exit(1);
    }

    return commands.generatePrivateKey(keyName);
  }

  console.log(`Unknown arg: ${arg}\n`);
  process.exit(1);
})();
