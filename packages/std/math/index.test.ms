let math := import "@std/math";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("math");
    testing.it("'abs' keeps positive ints", testing.toBe(math.abs(3), 3));
    testing.it("'abs' flips negative ints", testing.toBe(math.abs(- 3), 3));
    testing.it(
      "'signInt' returns -1 for negatives",
      testing.toBe(math.signInt(- 7), - 1),
    );
    testing.it("'signInt' returns 0 for zero", testing.toBe(math.signInt(0), 0));
    testing.it(
      "'signInt' returns 1 for positives",
      testing.toBe(math.signInt(7), 1),
    );
    testing.it("'max' picks larger value", testing.toBe(math.max(2, 5), 5));
    testing.it("'min' picks smaller value", testing.toBe(math.min(2, 5), 2));
    testing.it(
      "'clamp' keeps in-range value",
      testing.toBe(math.clamp(5, 0, 10), 5),
    );
    testing.it(
      "'clamp' clamps below low",
      testing.toBe(math.clamp(- 1, 0, 10), 0),
    );
    testing.it(
      "'clamp' clamps above high",
      testing.toBe(math.clamp(11, 0, 10), 10),
    );
    testing.endDescribe()
  );
