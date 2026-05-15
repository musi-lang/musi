let sys := import "#sys";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("sys");
    testing.it("'os' is Some", sys.os /= "");
    testing.it("'arch' is Some", sys.arch /= "");
    testing.it("'archFamily' is Some", sys.archFamily /= "",);
    testing.it("'family' is Some", sys.family /= "");
    testing.it("'pointerWidth' is supported", sys.pointerWidth > 0,);
    testing.it("'matchesArch' ignores casing", sys.matchesArch(sys.arch),);
    testing.it("'matchesOs' ignores casing", sys.matchesOs(sys.os),);
    testing.endDescribe()
  );
