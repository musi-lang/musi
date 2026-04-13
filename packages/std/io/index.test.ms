let Testing := import "@std/testing";
let Io := import "./index.ms";

export let test () :=
  (
    Testing.describe("io/index");
    Testing.it("'write' returns without failure", (Io.write("std-io"); 0 = 0));
    Testing.it("'writeLine' returns without failure", (Io.writeLine("std-io"); 0 = 0));
    Testing.it("'writeError' returns without failure", (Io.writeError("std-io"); 0 = 0));
    Testing.it("'writeErrorLine' returns without failure", (Io.writeErrorLine("std-io"); 0 = 0));
    Testing.endDescribe()
  );
