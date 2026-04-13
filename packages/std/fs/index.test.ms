let Testing := import "@std/testing";
let Path := import "@std/path";
let Fs := import "./index.ms";

let filePath := Path.join("target", "musi-std-fs-test.txt");

export let test () :=
  (
    Testing.describe("fs/index");
    Testing.it("'createDirAll' creates target dir", Testing.toBeTruthy(Fs.createDirAll("target")));
    Testing.it("'writeText' stores file", (Fs.writeText(filePath, "hello"); 0 = 0));
    Testing.it("'exists' detects written file", Testing.toBeTruthy(Fs.exists(filePath)));
    Testing.it("'readText' returns file content", Testing.toBeTruthy(Fs.readText(filePath) = "hello"));
    Testing.it("'remove' deletes file", Testing.toBeTruthy(Fs.remove(filePath)));
    Testing.endDescribe()
  );
