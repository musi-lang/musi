let Testing := import "@std/testing";
let Assert := import "@std/assert";

export let test () :=
  (
    Testing.describe("assert/matchers");
    Testing.it("'equal' passes on equal ints", Assert.equal[Int](4, 4));
    Testing.it("'notEqual' passes on different ints", Assert.notEqual[Int](4, 3));
    Testing.it("'isTrue' passes on comparison", Assert.isTrue(1 = 1));
    Testing.it("'isFalse' passes on comparison", Assert.isFalse(1 = 2));
    Testing.it("'greaterThan' matcher passes", Assert.greaterThan(5, 2));
    Testing.it("'lessThan' matcher passes", Assert.lessThan(2, 5));
    Testing.it("'betweenInclusive' matcher passes", Assert.betweenInclusive(3, 0, 4));
    Testing.endDescribe()
  );
