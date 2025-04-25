import { join } from "jsr:@std/path";

const [startIndex, endIndex] = Deno.args.map(Number);

if (isNaN(startIndex) || isNaN(endIndex) || startIndex > endIndex) {
  console.error("Usage: deno run script.ts <startIndex> <endIndex>");
  Deno.exit(1);
}

interface Test {
  input: {
    path: string;
    class: string;
  };
  output: string;
}

let passed = 0;
let failed = 0;
let total = 0;

async function runTest(num: number) {
  const numStr = num.toString().padStart(2, "0");

  const allTests: Test[] = [];
  for await (const testFile of Deno.readDir("input")) {
    if (testFile.isFile && testFile.name.endsWith("_in.js")) {
      const inputPath = join("input", testFile.name);

      let className = "A";
      const classNameMatch = testFile.name.match(/\d+_in_([a-zA-Z]+)/);
      if (classNameMatch) {
        className = classNameMatch[0];
      }

      allTests.push({
        input: { path: inputPath, class: className },
        output: join("input", testFile.name.replace(/_in_[a-zA-Z].js/, "_out")),
      });
    }
  }
  const inputPath = join("input", `test${numStr}_in.js`);
  const expectedPath = join("input", `test${numStr}_out`);

  const expectedOutput = await Deno.readTextFile(expectedPath);
  const expectedError = expectedOutput.trim() === "error";

  const child = new Deno.Command("racket", {
    args: [],
    stdin: "piped",
    stdout: "piped",
    stderr: "piped",
  }).spawn();

  const writer = child.stdin.getWriter();
  await writer.write(new TextEncoder().encode(inputPath));
  writer.releaseLock();
  await child.stdin.close();

  const { stdout, stderr, code } = await child.output();

  const output = new TextDecoder().decode(stdout).trim();
  const errorOutput = new TextDecoder().decode(stderr).trim();

  if (expectedError) {
    if (code !== 0) {
      console.log("PASS");
      passed++;
    } else {
      console.log("FAIL");
      console.log("Expected error with non-zero exit code");
      console.log(`Got exit code: ${code}`);
      console.log(`Output: ${output}`);
      console.log(`Error Output: ${errorOutput}\n`);
      failed++;
    }
  } else {
    if (output === expectedOutput.trim() && code === 0) {
      console.log("PASS");
      passed++;
    } else {
      console.log("FAIL");
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
    await runTest(i);
  }

  console.log("\nSummary:");
  console.log(`Passed: ${passed}`);
  console.log(`Failed: ${failed}`);
  console.log(`Total:  ${total}`);

  Deno.exit(failed === 0 ? 0 : 1);
}

main();
