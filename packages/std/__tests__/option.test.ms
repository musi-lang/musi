let option := import "@std/option";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("option");
    let someOne := option.someOf[Int](1);
    let noneInt := option.noneOf[Int]();
    testing.it("'isSome' detects present value", someOne.isSome(),);
    testing.it("'isNone' detects missing value", noneInt.isNone(),);
    testing.it(
      "'unwrapOr' returns fallback for none",
      testing.toBe(noneInt.unwrapOr(7), 7),
    );
    testing.it(
      "'unwrapOrElse' computes fallback for none",
      testing.toBe(noneInt.unwrapOrElse(9), 9),
    );
    testing.it(
      "'map' transforms present values",
      testing.toBe(
        option.unwrapOr[Int](option.map[Int, Int](someOne, \(
              x
            ) => x + 1), 0),
        2,
      ),
    );
    testing.it(
      "'fold' maps present values",
      testing.toBe(option.fold[Int, Int](someOne, 0, \(x) => x + 2), 3),
    );
    testing.endDescribe()
  );
