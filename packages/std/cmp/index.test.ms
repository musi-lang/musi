let Testing := import "@std/testing";
let Cmp := import "./index.ms";

export let test () :=
  (
    Testing.describe("cmp");
    Testing.it("'eq' compares equal values", Testing.toBeTruthy(Cmp.eq[Int](3, 3)));
    Testing.it("'compareInt' returns less", Testing.toBeTruthy(Cmp.isLess(Cmp.compareInt(1, 2))));
    Testing.it("'compareInt' returns equal", Testing.toBeTruthy(Cmp.isEqual(Cmp.compareInt(4, 4))));
    Testing.it("'compareInt' returns greater", Testing.toBeTruthy(Cmp.isGreater(Cmp.compareInt(5, 4))));
    Testing.it("'clampInt' clamps values", Testing.toBe(Cmp.clampInt(11, 0, 10), 10));
    Testing.endDescribe()
  );
