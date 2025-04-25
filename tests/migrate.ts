import { walk } from "jsr:@std/fs";
import { join } from "jsr:@std/path";

for await (const file of walk("./input")) {
  if (file.isDirectory) continue;
  if (file.name.includes("out")) continue;

  const content = await Deno.readTextFile(file.path);
  let newContent = `class A {
    ${content}
  }`;
  newContent = newContent.replace(/function main/, "static function main");
  await Deno.writeTextFile(join("output", file.name), newContent);
}
