let Testing := import "@std/testing";
let Path := import "./index.ms";

export let test () :=
  (
    Testing.describe("path/index");
    Testing.it("'join' combines segments", Testing.toBeTruthy(Path.join("a", "b") /= ""));
    Testing.it("'normalize' keeps relative file name", Testing.toBeTruthy(Path.normalize("a/./b") /= ""));
    Testing.it("'dirname' returns parent", Testing.toBeTruthy(Path.dirname("dir/file.txt") = "dir"));
    Testing.it("'basename' returns file name", Testing.toBeTruthy(Path.basename("dir/file.txt") = "file.txt"));
    Testing.it("'extname' returns extension", Testing.toBeTruthy(Path.extname("dir/file.txt") = ".txt"));
    Testing.it("'isAbsolute' detects absolute path", Testing.toBeTruthy(Path.isAbsolute("/x")));
    Testing.endDescribe()
  );
