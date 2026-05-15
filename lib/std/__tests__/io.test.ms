let io := import "@std/io";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("io");
    testing.it("'write' returns without failure",
        (
          io.write("std-io");
          0 = 0
        ));
    testing.it(
      "'writeLn' returns without failure",
        (
          io.writeLn("std-io");
          0 = 0
        ),
    );
    testing.it(
      "'writeErr' returns without failure",
        (
          io.writeErr("std-io");
          0 = 0
        ),
    );
    testing.it(
      "'writeErrLn' returns without failure",
        (
          io.writeErrLn("std-io");
          0 = 0
        ),
    );
    testing.endDescribe()
  );
