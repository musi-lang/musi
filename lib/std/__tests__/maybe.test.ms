let assert := import "@std/assert";
let maybe := import "@std/maybe";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("maybe");
    let someOne := maybe.Some[Int](1);
    let noneInt := maybe.None[Int]();
    testing.it("'isSome' detects Some value", someOne.isSome(),);
    testing.it("'isNone' detects None value", noneInt.isNone(),);
    testing.it(
      "'unwrapOr' returns fallback for None",
      assert.toBe(noneInt.unwrapOr(7), 7),
    );
    testing.it(
      "'unwrapOrElse' computes fallback for None",
      assert.toBe(noneInt.unwrapOrElse(\() => 9), 9),
    );
    testing.it(
      "'unwrapOrElse' skips fallback for Some",
      assert.toBe(someOne.unwrapOrElse(\() => 9), 1),
    );
    testing.it(
      "'map' transforms Some values",
      assert.toBe(
        maybe.unwrapOr[Int](maybe.map[Int, Int](someOne, \(x) => x + 1), 0),
        2,
      ),
    );
    testing.it(
      "'fold' maps Some values",
      assert.toBe(maybe.fold[Int, Int](someOne, 0, \(x) => x + 2), 3),
    );
    testing.it(
      "'mapOrElse' computes fallback for None",
      assert.toBe(
        maybe.mapOrElse[Int, Int](noneInt, \() => 8, \(x) => x + 1),
        8,
      ),
    );
    testing.it(
      "'mapOrElse' maps Some values",
      assert.toBe(
        maybe.mapOrElse[Int, Int](someOne, \() => 8, \(x) => x + 1),
        2,
      ),
    );
    testing.endDescribe()
  );
