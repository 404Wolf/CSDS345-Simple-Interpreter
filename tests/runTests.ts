import { join } from "jsr:@std/path";

const DEFAULT_CLASS = "A";
const SRC_PATH = join("src", "interpreter.rkt")

const [startIndex, endIndex] = Deno.args.map(Number);

if (isNaN(startIndex) || isNaN(endIndex) || startIndex > endIndex) {
  console.error("Usage: deno run script.ts <startIndex> <endIndex>");
  Deno.exit(1);
}

let passed = 0;
let failed = 0;
let total = 0;

async function executeTest(inputPath: string, expectedPath: string) {
  const expectedOutput = await Deno.readTextFile(expectedPath);
  const expectedError = expectedOutput.trim() === "error";

  // Extract the file basename
  const filename = inputPath.split("/").pop() || "";

  // Check if there's a class name after "_in"
  const suffixMatch = filename.match(/_in_([^.]+)\.js$/);
  const className = suffixMatch ? suffixMatch[1] : DEFAULT_CLASS;

  const child = new Deno.Command("racket", {
    args: [SRC_PATH, join("tests", "input", filename), className],
    stdout: "piped",
    stderr: "piped",
  }).spawn();

  const { stdout, stderr, code } = await child.output();

  const output = new TextDecoder().decode(stdout).trim();
  const errorOutput = new TextDecoder().decode(stderr).trim();

  return { output, errorOutput, code, expectedOutput, expectedError };
}

async function runTest(num: number) {
  const numStr = num.toString().padStart(2, "0");
  const inputPath = join("tests", "input", `test${numStr}_in.js`);
  const expectedPath = join("tests", "input", `test${numStr}_out`);

  const { output, errorOutput, code, expectedOutput, expectedError } =
    await executeTest(inputPath, expectedPath);

  if (expectedError) {
    if (code !== 0) {
      console.log(`Test ${num}: PASS`);
      passed++;
    } else {
      console.log(`Test ${num}: FAIL`);
      console.log("Expected error with non-zero exit code");
      console.log(`Got exit code: ${code}`);
      console.log(`Output: ${output}`);
      console.log(`Error Output: ${errorOutput}\n`);
      failed++;
    }
  } else {
    if (output === expectedOutput.trim() && code === 0) {
      console.log(`Test ${num}: PASS`);
      passed++;
    } else {
      console.log(`Test ${num}: FAIL`);
      console.log("Expected:");
      console.log(expectedOutput);
      console.log("\nGot:");
      console.log(output);
      console.log(`Error Output: ${errorOutput}\n`);
      failed++;
    }
  }
  total++;
}

async function main() {
  for (let i = startIndex; i <= endIndex; i++) {
    await runTest(i)
  }

  console.log("\nSummary:");
  console.log(`Passed: ${passed}`);
  console.log(`Failed: ${failed}`);
  console.log(`Total:  ${total}`);

  Deno.exit(failed === 0 ? 0 : 1);
}

main();
