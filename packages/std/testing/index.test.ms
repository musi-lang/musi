let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("testing");
    testing.it("'pass' returns outcome", testing.toBe(1, 1));
    testing.it("'fail' returns false", testing.toBeFalsy(testing.fail("boom")));
    testing.endDescribe()
  );
