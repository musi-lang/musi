let sys := import "#sys";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("sys");
    testing.it("'os' is present", sys.os /= "");
    testing.it("'arch' is present", sys.arch /= "");
    testing.it("'archFamily' is present", sys.archFamily /= "",);
    testing.it("'family' is present", sys.family /= "");
    testing.it("'pointerWidth' is supported", sys.pointerWidth > 0,);
    testing.it("'matchesArch' ignores casing", sys.matchesArch(sys.arch),);
    testing.it("'matchesOs' ignores casing", sys.matchesOs(sys.os),);
    testing.it(
      "'supportsJit' mirrors jit record",
      sys.supportsJit() = sys.jit.supported,
    );
    testing.endDescribe()
  );
