let cmp := import "./index.ms";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("cmp");
    testing.it(
      "'eq' compares equal values",
      testing.toBeTruthy(cmp.eq[Int](3, 3)),
    );
    testing.it(
      "'compareInt' returns less",
      testing.toBeTruthy(cmp.compareInt(1, 2).isLess()),
    );
    testing.it(
      "'compareInt' returns equal",
      testing.toBeTruthy(cmp.compareInt(4, 4).isEqual()),
    );
    testing.it(
      "'compareInt' returns greater",
      testing.toBeTruthy(cmp.compareInt(5, 4).isGreater()),
    );
    testing.it(
      "'compareString' returns less",
      testing.toBeTruthy(cmp.compareString("a", "b").isLess()),
    );
    testing.it(
      "'compareString' returns equal",
      testing.toBeTruthy(cmp.compareString("a", "a").isEqual()),
    );
    testing.it(
      "'compareString' returns greater",
      testing.toBeTruthy(cmp.compareString("b", "a").isGreater()),
    );
    testing.it(
      "'compareString' uses unicode scalar order",
      testing.toBeTruthy(cmp.compareString("é", "z").isGreater()),
    );
    testing.it(
      "'compareFloat' orders positive infinity",
      testing.toBeTruthy(cmp.compareFloat(1.0 / 0.0, 1.0).isGreater()),
    );
    testing.it(
      "'compareFloat' orders negative infinity",
      testing.toBeTruthy(cmp.compareFloat(- 1.0 / 0.0, - 1.0).isLess()),
    );
    testing.it(
      "'compareFloat' orders signed zero",
      testing.toBeTruthy(cmp.compareFloat(0.0 / - 1.0, 0.0).isLess()),
    );
    testing.it(
      "'compareFloat' orders NaN deterministically",
      testing.toBeTruthy(cmp.compareFloat(0.0 / 0.0, 0.0 / 0.0).isEqual()),
    );
    testing.it(
      "'clampInt' clamps values",
      testing.toBe(cmp.clampInt(11, 0, 10), 10),
    );
    testing.endDescribe()
  );
