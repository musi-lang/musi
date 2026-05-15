let path := import "@std/path";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("path/index");
    let dir := path.fromString("dir");
    let file := path.fromString("file.txt");
    let joined := dir.join(file);
    testing.it("'join' combines segments", path.toString(joined) /= "",);
    testing.it(
      "'normalize' keeps relative file name",
      path.toString(path.normalize(path.fromString("a/./b"))) /= "",
    );
    testing.it(
      "'dirName' returns parent",
      path.toString(path.dirName(path.fromString("dir/file.txt"))) = "dir",
    );
    testing.it(
      "'baseName' returns file name",
      path.fromString("dir/file.txt").baseName() = "file.txt",
    );
    testing.it(
      "'extName' returns extension",
      path.fromString("dir/file.txt").extName() = ".txt",
    );
    testing.it(
      "'isAbsolute' detects absolute path",
      path.fromString("/x").isAbsolute(),
    );
    testing.endDescribe()
  );
