let Testing := import "@std/testing";

export let test () :=
  (
    Testing.describe("testing");
    Testing.it("pass returns outcome", Testing.toBe(1, 1));
    Testing.it("fail returns false", Testing.fail("boom"));
    Testing.endDescribe()
  );
