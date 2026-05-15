let os := import "@std/os";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("os/index");
    let currentOs := os.targetOs(os.current);
    let currentArch := os.targetArch(os.current);
    let currentFamily := os.targetFamily(os.current);
    testing.it("current OS is Some", currentOs.toString() /= "",);
    testing.it("current arch is Some", currentArch.toString() /= "",);
    testing.it("current family is Some", currentFamily.toString() /= "",);
    testing.it(
      "pointer width is supported",
      os.targetPointerWidth(os.current) > 0,
    );
    testing.it("'matchesOs' accepts typed OS", os.matchesOs(currentOs),);
    testing.it("'matchesArch' accepts typed arch", os.matchesArch(currentArch),);
    testing.endDescribe()
  );
