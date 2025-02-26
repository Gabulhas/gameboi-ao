import path from "node:path";
import fs from "node:fs";
import { execSync } from "node:child_process";

export default class LuaLoader {
  protected lua_path: string;

  constructor(luaPath: string = "") {
    this.lua_path = luaPath;
  }

  loadProcess(processPath: string) {
    const resolvedProcessPath = path.resolve(process.cwd(), processPath);
    const outputFile = path.resolve(
      path.dirname(resolvedProcessPath),
      "compiled.lua",
    );
    const initialCwd = process.cwd();

    try {
      process.chdir(path.dirname(resolvedProcessPath));
      execSync(
        `LUA_PATH="${this.lua_path}" lua -lamalg "${resolvedProcessPath}"`,
        {
          stdio: "inherit",
          encoding: "utf-8",
        },
      );

      execSync(
        `LUA_PATH="${this.lua_path}" amalg.lua -o "${outputFile}" -s "${resolvedProcessPath}" -c`,
        {
          stdio: "inherit",
          encoding: "utf-8",
        },
      );

      process.chdir(initialCwd);
      console.log(`Successfully created amalgamated file at: ${outputFile}`);
      return fs.readFileSync(outputFile, "utf-8");
    } catch (error) {
      process.chdir(initialCwd);

      console.error("Amalgamation failed:", error);
      throw error;
    }
  }
}
