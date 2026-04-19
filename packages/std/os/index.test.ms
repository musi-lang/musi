let os := import "@std/os";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("os/index");
    let currentOs := os.targetOs(os.current);
    let currentArch := os.targetArch(os.current);
    let currentFamily := os.targetFamily(os.current);
    testing.it(
      "current OS is present",
      testing.toBeTruthy(currentOs.toString() /= ""),
    );
    testing.it(
      "current arch is present",
      testing.toBeTruthy(currentArch.toString() /= ""),
    );
    testing.it(
      "current family is present",
      testing.toBeTruthy(currentFamily.toString() /= ""),
    );
    testing.it(
      "pointer width is supported",
      testing.toBeTruthy(os.targetPointerWidth(os.current) > 0),
    );
    testing.it(
      "'matchesOs' accepts typed OS",
      testing.toBeTruthy(os.matchesOs(currentOs)),
    );
    testing.it(
      "'matchesArch' accepts typed arch",
      testing.toBeTruthy(os.matchesArch(currentArch)),
    );
    testing.it(
      "'supportsJit' mirrors typed jit",
      testing.toBeTruthy(os.supportsJit() = os.currentJit.isSupported()),
    );
    testing.endDescribe()
  );
