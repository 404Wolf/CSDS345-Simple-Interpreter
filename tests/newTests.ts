import { join } from "jsr:@std/path";

const OFFSET = 65;

const fileContent = await Deno.readTextFile("./tests/newTests.html");
const testRegex =
  /Test (?<testNum>\d+) should return (?<expectedOutput>-?\w+) when run(?:ning)?(?:with)? (?<testClass>\w+)'s main\.\s*(?<testBody>[\s\S]*?)(?=Test \d+|$)/g;

const matches = Array.from(fileContent.matchAll(testRegex));

for (const match of matches) {
  const {
    testNum: testNumStr,
    expectedOutput,
    testClass,
    testBody: newTestContent,
  } = match
    .groups!;

  const testNum = Number(testNumStr) + OFFSET;

  await Deno.writeTextFile(
    join("testss", `test${testNum}_in_${testClass}.js`),
    newTestContent,
  );
  await Deno.writeTextFile(
    join("testss", `test${testNum}_out`),
    expectedOutput,
  );
}
