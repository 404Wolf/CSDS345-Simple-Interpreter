import { walk } from "jsr:@std/fs";
import { join } from "jsr:@std/path";

const newtests = await Deno.readTextFile("newTests.html");
