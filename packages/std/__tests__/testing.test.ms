let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("testing");
    testing.it("'pass' returns outcome", testing.toBe(1, 1));
    testing.it("'fail' returns false", testing.toBeFalse(testing.fail("boom")));
    testing.it("'toBeTrue' accepts true", testing.toBeTrue(1 = 1));
    testing.it("'toBeFalse' accepts false", testing.toBeFalse(1 = 2));
    testing.endDescribe()
  );
