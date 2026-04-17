let testing := import "@std/testing";
let option := import "@std/option";

export let test () :=
  (
    testing.describe("option");
    let someOne := option.some[Int](1);
    let noneInt := option.none[Int]();
    testing.it("'isSome' detects present value", testing.toBeTruthy(option.isSome[Int](someOne)));
    testing.it("'isNone' detects missing value", testing.toBeTruthy(option.isNone[Int](noneInt)));
    testing.it("'unwrapOr' returns fallback for none", testing.toBe(option.unwrapOr[Int](noneInt, 7), 7));
    testing.it("'unwrapOrElse' computes fallback for none", testing.toBe(option.unwrapOrElse[Int](noneInt, 9), 9));
    testing.it("'map' transforms present values", testing.toBe(option.unwrapOr[Int](option.map[Int, Int](someOne, \(x) => x + 1), 0), 2));
    testing.it("'fold' maps present values", testing.toBe(option.fold[Int, Int](someOne, 0, \(x) => x + 2), 3));
    testing.endDescribe()
  );
