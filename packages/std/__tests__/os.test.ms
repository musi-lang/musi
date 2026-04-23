let os := import "@std/os";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("os/index");
    let currentOs := os.targetOs(os.current);
    let currentArch := os.targetArch(os.current);
    let currentFamily := os.targetFamily(os.current);
    testing.it("current OS is present", currentOs.toString() /= "",);
    testing.it("current arch is present", currentArch.toString() /= "",);
    testing.it("current family is present", currentFamily.toString() /= "",);
    testing.it(
      "pointer width is supported",
      os.targetPointerWidth(os.current) > 0,
    );
    testing.it("'matchesOs' accepts typed OS", os.matchesOs(currentOs),);
    testing.it("'matchesArch' accepts typed arch", os.matchesArch(currentArch),);
    testing.it(
      "'supportsJit' mirrors typed jit",
      os.supportsJit() = os.currentJit.isSupported(),
    );
    testing.endDescribe()
  );
