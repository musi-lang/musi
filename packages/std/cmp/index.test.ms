let testing := import "@std/testing";
let cmp := import "./index.ms";

export let test () :=
  (
    testing.describe("cmp");
    testing.it("'eq' compares equal values", testing.toBeTruthy(cmp.eq[Int](3, 3)));
    testing.it("'compareInt' returns less", testing.toBeTruthy(cmp.isLess(cmp.compareInt(1, 2))));
    testing.it("'compareInt' returns equal", testing.toBeTruthy(cmp.isEqual(cmp.compareInt(4, 4))));
    testing.it("'compareInt' returns greater", testing.toBeTruthy(cmp.isGreater(cmp.compareInt(5, 4))));
    testing.it("'compareString' returns less", testing.toBeTruthy(cmp.isLess(cmp.compareString("a", "b"))));
    testing.it("'compareString' returns equal", testing.toBeTruthy(cmp.isEqual(cmp.compareString("a", "a"))));
    testing.it("'compareString' returns greater", testing.toBeTruthy(cmp.isGreater(cmp.compareString("b", "a"))));
    testing.it("'compareString' uses unicode scalar order", testing.toBeTruthy(cmp.isGreater(cmp.compareString("é", "z"))));
    testing.it("'compareFloat' orders positive infinity", testing.toBeTruthy(cmp.isGreater(cmp.compareFloat(1.0 / 0.0, 1.0))));
    testing.it("'compareFloat' orders negative infinity", testing.toBeTruthy(cmp.isLess(cmp.compareFloat(-1.0 / 0.0, -1.0))));
    testing.it("'compareFloat' orders signed zero", testing.toBeTruthy(cmp.isLess(cmp.compareFloat(0.0 / -1.0, 0.0))));
    testing.it("'compareFloat' orders NaN deterministically", testing.toBeTruthy(cmp.isEqual(cmp.compareFloat(0.0 / 0.0, 0.0 / 0.0))));
    testing.it("'clampInt' clamps values", testing.toBe(cmp.clampInt(11, 0, 10), 10));
    testing.endDescribe()
  );
