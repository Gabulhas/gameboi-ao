import { readFileSync, writeFileSync } from "fs";
import * as aoconnect from "@permaweb/aoconnect";
import sharp from "sharp";
import config from "../../deploy.config.ts";

async function convertToPNG(base64Data: string) {
  const buffer = Buffer.from(base64Data, "base64");
  await sharp(buffer, {
    raw: {
      width: 160,
      height: 144,
      channels: 3,
    },
  })
    .toFormat("png")
    .toFile("output.png");
}

(async () => {
  const processId = process.argv[2];

  const processConfigs = config["gbc_process"];

  let wallet = processConfigs.wallet;

  if (typeof wallet === "string") {
    wallet = JSON.parse(
      readFileSync(wallet).toString(),
    );
  }

  const signer = aoconnect.createDataItemSigner(wallet);

  await aoconnect.message({
    process: processId,
    tags: [
      { name: "Action", value: "RunVBlank" },
    ],
    signer,
  });

  const result = await aoconnect.dryrun({
    process: processId,
    tags: [
      { name: "Action", value: "GetScreen" },
    ],
  });

  const responseData = JSON.parse(result.Output.data) as any;
  const imageBase64 = responseData.data.body;
  console.log({ imageBase64 });
  convertToPNG(imageBase64);
})();
