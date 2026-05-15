let assert := import "@std/assert";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("testing");
    testing.it("'it' records a passing result", assert.toBeTrue(1 = 1));
    testing.endDescribe()
  );
