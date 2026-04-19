let assert := import "@std/assert";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("assert/matchers");
    testing.it("'equal' passes on equal ints", assert.equal[Int](4, 4));
    testing.it(
      "'notEqual' passes on different ints",
      assert.notEqual[Int](4, 3),
    );
    testing.it("'isTrue' passes on comparison", assert.isTrue(1 = 1));
    testing.it("'isFalse' passes on comparison", assert.isFalse(1 = 2));
    testing.it("'greaterThan' matcher passes", assert.greaterThan(5, 2));
    testing.it("'lessThan' matcher passes", assert.lessThan(2, 5));
    testing.it(
      "'betweenInclusive' matcher passes",
      assert.betweenInclusive(3, 0, 4),
    );
    testing.endDescribe()
  );
