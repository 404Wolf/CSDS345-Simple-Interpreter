import { basename, join } from "jsr:@std/path";
import { parseArgs } from "jsr:@std/cli/parse-args";
import { green, red } from "jsr:@std/fmt/colors";

const RACKET_FLAGS = ["-d", "-l", "errortrace", "-t"];
const DEFAULT_CLASS = "A";
const SRC_PATH = join("src", "interpreter.rkt");
const TEST_DIR_CONTENTS = await Array.fromAsync(
  Deno.readDir(join("tests", "input")),
);

const parsedArgs = parseArgs(Deno.args, {
  boolean: ["concurrent"],
  alias: { concurrent: "c" },
});

const concurrentMode = parsedArgs.concurrent || false;
let [startIndex, endIndex] = parsedArgs._.map(Number);

if (isNaN(startIndex) || startIndex > endIndex) {
  console.error(
    "Usage: deno run script.ts [--concurrent] <startIndex> <endIndex>",
  );
  Deno.exit(1);
}

if (startIndex && !endIndex) {
  endIndex = startIndex;
}

let passed = 0;
let failed = 0;

async function executeTest(inputPath: string, expectedPath: string) {
  const expectedOutput = await Deno.readTextFile(expectedPath);
  const expectedError = expectedOutput.trim() === "error";

  const filename = basename(inputPath);
  const suffixMatch = filename.match(/_in_([^.]+)\.js$/);
  const className = suffixMatch ? suffixMatch[1] : DEFAULT_CLASS;

  const child = new Deno.Command("racket", {
    args: [
      ...RACKET_FLAGS,
      SRC_PATH,
      join("tests", "input", filename),
      className,
    ],
    stdout: "piped",
    stderr: "piped",
  }).spawn();

  const { stdout, stderr, code } = await child.output();

  const output = new TextDecoder().decode(stdout).trim();
  const errorOutput = new TextDecoder().decode(stderr).trim();

  return { output, errorOutput, code, expectedOutput, expectedError };
}

function getTestResultMessage(
  num: number,
  isPassed: boolean,
  details: string = "",
) {
  if (isPassed) {
    return `Test ${num}: ${green("PASS")}`;
  } else {
    return `Test ${num}: ${red("FAIL")}\n${details}`;
  }
}

async function runTest(num: number) {
  const numStr = num.toString().padStart(2, "0");

  const inputPath = TEST_DIR_CONTENTS
    .find((file) => file.name.startsWith(`test${numStr}_in`))
    ?.name!;

  const expectedPath = join("tests", "input", `test${numStr}_out`);

  const { output, errorOutput, code, expectedOutput, expectedError } =
    await executeTest(inputPath, expectedPath);

  let isPassed = false;
  let details = "";

  if (expectedError) {
    if (code !== 0) {
      isPassed = true;
    } else {
      details =
        `Expected error with non-zero exit code\nGot exit code: ${code}\nOutput: ${output}\nError Output: ${errorOutput}\n`;
    }
  } else {
    if (output === expectedOutput.trim() && code === 0) {
      isPassed = true;
    } else {
      details =
        `Expected:\n${expectedOutput}\n\nGot:\n${output}\nError Output: ${errorOutput}\n`;
    }
  }

  const message = getTestResultMessage(num, isPassed, details);
  console.log(message);

  if (isPassed) {
    passed++;
  } else {
    failed++;
  }

  return isPassed;
}

async function main() {
  if (concurrentMode) {
    console.log("Running tests concurrently...");
    const testPromises = [];
    for (let i = startIndex; i <= endIndex; i++) {
      testPromises.push(runTest(i));
    }

    await Promise.all(testPromises);
  } else {
    for (let i = startIndex; i <= endIndex; i++) {
      await runTest(i);
    }
  }

  const total = endIndex - startIndex + 1;

  console.log("\nSummary:");
  console.log(`Passed: ${green(passed.toString())}`);
  console.log(`Failed: ${red(failed.toString())}`);
  console.log(`Total:  ${total}`);

  Deno.exit(failed === 0 ? 0 : 1);
}

main();
