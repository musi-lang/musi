let fs := import "@std/fs";
let path := import "@std/path";
let testing := import "@std/testing";

let targetDir := path.fromString(".musi-std-fs-test");
let filePath := targetDir.join(path.fromString("musi-std-fs-test.txt"));

export let test () :=
  (
    testing.describe("fs");
    testing.it("'createDirAll' creates target dir", fs.createDirAll(targetDir),);
    testing.it(
      "'writeText' stores file",
        (
          fs.writeText(filePath, "hello");
          0 = 0
        ),
    );
    testing.it("'exists' detects written file", fs.exists(filePath),);
    testing.it(
      "'readText' returns file content",
      fs.readText(filePath) = "hello",
    );
    testing.it("'remove' deletes file", fs.remove(filePath));
    testing.it("'remove' deletes directory", fs.remove(targetDir));
    testing.endDescribe()
  );
