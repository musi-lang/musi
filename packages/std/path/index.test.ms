let testing := import "@std/testing";
let path := import "./index.ms";

export let test () :=
  (
    testing.describe("path/index");
    let dir := path.fromString("dir");
    let file := path.fromString("file.txt");
    let joined := path.join(dir, file);
    testing.it("'join' combines segments", testing.toBeTruthy(path.toString(joined) /= ""));
    testing.it("'normalize' keeps relative file name", testing.toBeTruthy(path.toString(path.normalize(path.fromString("a/./b"))) /= ""));
    testing.it("'dirName' returns parent", testing.toBeTruthy(path.toString(path.dirName(path.fromString("dir/file.txt"))) = "dir"));
    testing.it("'baseName' returns file name", testing.toBeTruthy(path.baseName(path.fromString("dir/file.txt")) = "file.txt"));
    testing.it("'extName' returns extension", testing.toBeTruthy(path.extName(path.fromString("dir/file.txt")) = ".txt"));
    testing.it("'isAbsolute' detects absolute path", testing.toBeTruthy(path.isAbsolute(path.fromString("/x"))));
    testing.endDescribe()
  );
