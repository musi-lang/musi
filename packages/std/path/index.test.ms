let testing := import "@std/testing";
let path := import "./index.ms";

export let test () :=
  (
    testing.describe("path/index");
    testing.it("'join' combines segments", testing.toBeTruthy(path.join("a", "b") /= ""));
    testing.it("'normalize' keeps relative file name", testing.toBeTruthy(path.normalize("a/./b") /= ""));
    testing.it("'dirName' returns parent", testing.toBeTruthy(path.dirName("dir/file.txt") = "dir"));
    testing.it("'baseName' returns file name", testing.toBeTruthy(path.baseName("dir/file.txt") = "file.txt"));
    testing.it("'extName' returns extension", testing.toBeTruthy(path.extName("dir/file.txt") = ".txt"));
    testing.it("'isAbsolute' detects absolute path", testing.toBeTruthy(path.isAbsolute("/x")));
    testing.endDescribe()
  );
