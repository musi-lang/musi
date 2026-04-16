let testing := import "@std/testing";
let path := import "@std/path";
let fs := import "./index.ms";

let filePath := path.join("target", "musi-std-fs-test.txt");

export let test () :=
  (
    testing.describe("fs/index");
    testing.it("'createDirAll' creates target dir", testing.toBeTruthy(fs.createDirAll("target")));
    testing.it("'writeText' stores file", (fs.writeText(filePath, "hello"); 0 = 0));
    testing.it("'exists' detects written file", testing.toBeTruthy(fs.exists(filePath)));
    testing.it("'readText' returns file content", testing.toBeTruthy(fs.readText(filePath) = "hello"));
    testing.it("'remove' deletes file", testing.toBeTruthy(fs.remove(filePath)));
    testing.endDescribe()
  );
