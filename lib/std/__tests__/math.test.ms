let assert := import "@std/assert";
let math := import "@std/math";
let testing := import "@std/testing";

let close (actual : Float, expected : Float) : Bit :=
  math.absFloat(actual - expected) < 0.000001;

export let test () :=
  (
    testing.describe("math");
    testing.it("'abs' keeps positive ints", assert.toBe(math.abs(3), 3));
    testing.it("'abs' flips negative ints", assert.toBe(math.abs(- 3), 3));
    testing.it(
      "'signInt' returns -1 for negatives",
      assert.toBe(math.signInt(- 7), - 1),
    );
    testing.it("'signInt' returns 0 for zero", assert.toBe(math.signInt(0), 0));
    testing.it(
      "'signInt' returns 1 for positives",
      assert.toBe(math.signInt(7), 1),
    );
    testing.it("'max' picks larger value", assert.toBe(math.max(2, 5), 5));
    testing.it("'min' picks smaller value", assert.toBe(math.min(2, 5), 2));
    testing.it(
      "'clamp' keeps in-range value",
      assert.toBe(math.clamp(5, 0, 10), 5),
    );
    testing.it(
      "'clamp' clamps below low",
      assert.toBe(math.clamp(- 1, 0, 10), 0),
    );
    testing.it(
      "'clamp' clamps above high",
      assert.toBe(math.clamp(11, 0, 10), 10),
    );
    testing.it(
      "'absFloat' flips negative floats",
      close(math.absFloat(- 3.0), 3.0),
    );
    testing.it(
      "'maxFloat' picks larger float",
      close(math.maxFloat(1.0, 2.0), 2.0),
    );
    testing.it(
      "'minFloat' picks smaller float",
      close(math.minFloat(1.0, 2.0), 1.0),
    );
    testing.it(
      "'clampFloat' clamps above high",
      close(math.clampFloat(3.0, 0.0, 2.0), 2.0),
    );
    testing.endDescribe()
  );
