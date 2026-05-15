--! Testing module.

let intrinsics := import "musi:test";

--- Start a named test suite.
export let describe (name : String) := intrinsics.suiteStart(name);
--- End current test suite.
export let endDescribe () := intrinsics.suiteEnd();
--- Record one named test case.
export let it (name : String, passed : Bit) :=
  intrinsics.testCase(name, passed);
