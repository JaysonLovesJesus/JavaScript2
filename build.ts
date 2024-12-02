const stub_path = "./bin/stub.ts";
const stub = await import(stub_path);
import stdlib from "./stdlib/index.ts";

const input = await Deno.readTextFile("./src/main.ss");

console.log("Recompiling..");

let result: string = "";
try {
    result = stub.compile(input, stdlib);
    // const compiler = eval(result);
} catch (e) {
    console.log(e);
    Deno.exit(0);
}

await Deno.writeTextFile(stub_path, result);
console.log("Successfully recompiled into " + stub_path);
