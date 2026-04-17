let testing := import "@std/testing";
let os := import "@std/os";

export let test () :=
  (
    testing.describe("os/index");
    testing.it("current OS is present", testing.toBeTruthy(os.osName(os.targetOs(os.current)) /= ""));
    testing.it("current arch is present", testing.toBeTruthy(os.archName(os.targetArch(os.current)) /= ""));
    testing.it("current family is present", testing.toBeTruthy(os.familyName(os.targetFamily(os.current)) /= ""));
    testing.it("pointer width is supported", testing.toBeTruthy(os.targetPointerWidth(os.current) > 0));
    testing.it("'matchesOs' accepts typed OS", testing.toBeTruthy(os.matchesOs(os.targetOs(os.current))));
    testing.it("'matchesArch' accepts typed arch", testing.toBeTruthy(os.matchesArch(os.targetArch(os.current))));
    testing.it("'supportsJit' mirrors typed jit", testing.toBeTruthy(os.supportsJit() = os.isSupported(os.currentJit)));
    testing.endDescribe()
  );
