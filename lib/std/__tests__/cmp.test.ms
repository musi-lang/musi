let assert := import "@std/assert";
let cmp := import "@std/cmp";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("cmp");
    testing.it("'eq' compares equal values", cmp.eq[Int](3, 3),);
    testing.it("'compareInt' returns less", cmp.compareInt(1, 2).isLess(),);
    testing.it("'compareInt' returns equal", cmp.compareInt(4, 4).isEqual(),);
    testing.it(
      "'compareInt' returns greater",
      cmp.compareInt(5, 4).isGreater(),
    );
    testing.it(
      "'compareString' returns less",
      cmp.compareString("a", "b").isLess(),
    );
    testing.it(
      "'compareString' returns equal",
      cmp.compareString("a", "a").isEqual(),
    );
    testing.it(
      "'compareString' returns greater",
      cmp.compareString("b", "a").isGreater(),
    );
    testing.it(
      "'compareString' uses unicode scalar order",
      cmp.compareString("é", "z").isGreater(),
    );
    testing.it(
      "'compareFloat' returns less",
      cmp.compareFloat(1.25, 2.5).isLess(),
    );
    testing.it(
      "'compareFloat' returns equal",
      cmp.compareFloat(4.5, 4.5).isEqual(),
    );
    testing.it(
      "'compareFloat' returns greater",
      cmp.compareFloat(5.75, 4.5).isGreater(),
    );
    testing.it(
      "'clampInt' clamps values",
      assert.toBe(cmp.clampInt(11, 0, 10), 10),
    );
    testing.endDescribe()
  );
