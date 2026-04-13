let Testing := import "@std/testing";
let Math := import "@std/math";

export let test () :=
  (
    Testing.describe("math");
    Testing.it("'abs' keeps positive ints", Testing.toBe(Math.abs(3), 3));
    Testing.it("'abs' flips negative ints", Testing.toBe(Math.abs(-3), 3));
    Testing.it("'signInt' returns -1 for negatives", Testing.toBe(Math.signInt(-7), -1));
    Testing.it("'signInt' returns 0 for zero", Testing.toBe(Math.signInt(0), 0));
    Testing.it("'signInt' returns 1 for positives", Testing.toBe(Math.signInt(7), 1));
    Testing.it("'max' picks larger value", Testing.toBe(Math.max(2, 5), 5));
    Testing.it("'min' picks smaller value", Testing.toBe(Math.min(2, 5), 2));
    Testing.it("'clamp' keeps in-range value", Testing.toBe(Math.clamp(5, 0, 10), 5));
    Testing.it("'clamp' clamps below low", Testing.toBe(Math.clamp(-1, 0, 10), 0));
    Testing.it("'clamp' clamps above high", Testing.toBe(Math.clamp(11, 0, 10), 10));
    Testing.it("'clampFloat' keeps in-range value", Testing.toBeTruthy(Math.clampFloat(3.5, 1.0, 5.0) = 3.5));
    Testing.it("'clampFloat' clamps below low", Testing.toBeTruthy(Math.clampFloat(-1.0, 0.0, 1.0) = 0.0));
    Testing.it("'clampFloat' clamps above high", Testing.toBeTruthy(Math.clampFloat(2.0, 0.0, 1.0) = 1.0));
    Testing.endDescribe()
  );
