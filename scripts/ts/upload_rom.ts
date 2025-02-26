import { readFileSync } from "node:fs";

import config from "../../deploy.config.ts";
import * as aoconnect from "@permaweb/aoconnect";
import path from "path";
import { JWKInterface } from "arweave/node/lib/wallet";

const sleep = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

(async () => {
    const processId = process.argv[2];
    const romPath = process.argv[3];

    console.log({ processId, romPath: path.resolve(romPath) });

    const processConfigs = config["gbc_process"];

    let wallet = processConfigs.wallet;

    if (typeof wallet === "string") {
        wallet = JSON.parse(
            readFileSync(wallet).toString(),
        );
    }

    const signer = aoconnect.createDataItemSigner(wallet);

    console.log({ signer });
    const messageId = await aoconnect.message({
        process: processId,
        tags: [
            { name: "Action", value: "InitEmulator" },
        ],
        signer,
    });

    while (true) {
        const result = await aoconnect.result({
            process: processId,
            message: messageId,
        });
        if (result.Output) {
            break;
        }
    }

    console.log(romPath);
    const romBuffer = readFileSync(romPath);

    // In upload script:
    const base64String = romBuffer.toString("base64");

    console.log(
        `Size: Base64: ${base64String.length}, Bytes: ${romBuffer.length}`,
    );
    const messageIdLoad = await aoconnect.message({
        process: processId,
        tags: [
            { name: "Action", value: "LoadROM" },
            { name: "X-ROM-Name", value: path.basename(romPath) },
        ],
        data: base64String,
        signer,
    });

    console.log(messageIdLoad);
})();
