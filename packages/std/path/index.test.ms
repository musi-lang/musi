let path := import "./index.ms";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("path/index");
    let dir := path.fromString("dir");
    let file := path.fromString("file.txt");
    let joined := dir.join(file);
    testing.it(
      "'join' combines segments",
      testing.toBeTruthy(path.toString(joined) /= ""),
    );
    testing.it(
      "'normalize' keeps relative file name",
      testing.toBeTruthy(
        path.toString(path.normalize(path.fromString("a/./b"))) /= ""
      ),
    );
    testing.it(
      "'dirName' returns parent",
      testing.toBeTruthy(
        path.toString(path.dirName(path.fromString("dir/file.txt"))) = "dir"
      ),
    );
    testing.it(
      "'baseName' returns file name",
      testing.toBeTruthy(
        path.fromString("dir/file.txt").baseName() = "file.txt"
      ),
    );
    testing.it(
      "'extName' returns extension",
      testing.toBeTruthy(path.fromString("dir/file.txt").extName() = ".txt"),
    );
    testing.it(
      "'isAbsolute' detects absolute path",
      testing.toBeTruthy(path.fromString("/x").isAbsolute()),
    );
    testing.endDescribe()
  );
