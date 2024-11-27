import * as compiler from "../bin/stub.ts";

const res = compiler.compile("const a = 10;", {
    console: console, // uses log and error method
    error: function(msg: unknown) {
        console.error("Error: " + msg); // wat happens on a error
    }
});
console.log(res);
