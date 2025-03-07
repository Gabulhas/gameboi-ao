import express, { Express } from "express";
import path from "path";
import sharp from "sharp";
import * as aoconnect from "@permaweb/aoconnect";

const app: Express = express();
const PORT: number = 3000;

const processId = process.argv[2];

let cachedImage: Buffer;

async function fetchImage() {
  const result = await aoconnect.dryrun({
    process: processId,
    tags: [{ name: "Action", value: "GetScreen" }],
  });

  // Parse the result and extract the base64 image string
  const responseData = JSON.parse(result.Output.data);
  const imageBase64: string = responseData.data.body;

  // Convert the base64 image to a Buffer
  const buffer = Buffer.from(imageBase64, "base64");

  // Use sharp to convert the buffer to raw RGBA pixel data.
  // Note: We assume the original image is 160x144 with 3 channels (RGB).
  // Using .ensureAlpha() will add an alpha channel, giving you 4 channels per pixel.
  cachedImage = await sharp(buffer, {
    raw: { width: 160, height: 144, channels: 3 },
  })
    .ensureAlpha() // adds an alpha channel (i.e. converts to RGBA)
    .raw()
    .toBuffer();
}

app.get("/pixel-data", async (req, res) => {
  try {
    if (cachedImage) {
      res.set("Content-Type", "application/octet-stream");
      res.send(cachedImage);
    }
  } catch (error) {
    console.error("Error retrieving pixel data:", error);
    res.status(500).send("Error retrieving pixel data");
  }
});

app.use("/modules", express.static(path.join(__dirname, "../node_modules")));

app.use(express.static(path.join(__dirname, "./public")));

app.listen(PORT, () => {
  console.log(`Server running at http://localhost:${PORT}`);
  setInterval(fetchImage, 10 * 1000);
});
