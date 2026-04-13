let Testing := import "@std/testing";
let Math := import "@std/math";

export let test () :=
  (
    Testing.describe("math/min");
    Testing.it("picks smaller value", Testing.toBe(Math.min(2, 5), 2));
    Testing.endDescribe()
  );
