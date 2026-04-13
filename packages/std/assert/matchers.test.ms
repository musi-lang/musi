let Testing := import "@std/testing";
let Assert := import "@std/assert";

export let test () :=
  (
    Testing.describe("assert/matchers");
    Testing.it("'toBe' passes on equal ints", Assert.toBe(4, 4));
    Testing.it("'toBeTruthy' passes on comparison", Assert.toBeTruthy(1 = 1));
    Testing.it("'toBeFalsy' passes on comparison", Assert.toBeFalsy(1 = 2));
    Testing.it("'toBeGreaterThan' matcher passes", Assert.toBeGreaterThan(5, 2));
    Testing.it("'toBeLessThan' matcher passes", Assert.toBeLessThan(2, 5));
    Testing.endDescribe()
  );
