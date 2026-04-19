let sys := import "@std/sys";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("sys/index");
    testing.it("'os' is present", testing.toBeTruthy(sys.os /= ""));
    testing.it("'arch' is present", testing.toBeTruthy(sys.arch /= ""));
    testing.it(
      "'archFamily' is present",
      testing.toBeTruthy(sys.archFamily /= ""),
    );
    testing.it("'family' is present", testing.toBeTruthy(sys.family /= ""));
    testing.it(
      "'pointerWidth' is supported",
      testing.toBeTruthy(sys.pointerWidth > 0),
    );
    testing.it(
      "'matchesArch' ignores casing",
      testing.toBeTruthy(sys.matchesArch(sys.arch)),
    );
    testing.it(
      "'matchesOs' ignores casing",
      testing.toBeTruthy(sys.matchesOs(sys.os)),
    );
    testing.it(
      "'supportsJit' mirrors jit record",
      testing.toBeTruthy(sys.supportsJit() = sys.jit.supported),
    );
    testing.endDescribe()
  );
