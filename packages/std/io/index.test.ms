let testing := import "@std/testing";
let io := import "./index.ms";

export let test () :=
  (
    testing.describe("io/index");
    testing.it("'write' returns without failure", (io.write("std-io"); 0 = 0));
    testing.it("'writeLine' returns without failure", (io.writeLine("std-io"); 0 = 0));
    testing.it("'writeErr' returns without failure", (io.writeErr("std-io"); 0 = 0));
    testing.it("'writeErrLine' returns without failure", (io.writeErrLine("std-io"); 0 = 0));
    testing.endDescribe()
  );
