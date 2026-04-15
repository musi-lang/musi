let Testing := import "@std/testing";
let Option := import "@std/option";

export let test () :=
  (
    Testing.describe("option");
    Testing.it("'isSome' detects present value", Testing.toBeTruthy(Option.isSome[Int](Option.some[Int](1))));
    Testing.it("'isNone' detects missing value", Testing.toBeTruthy(Option.isNone[Int](Option.none[Int]())));
    Testing.it("'unwrapOr' returns fallback for none", Testing.toBe(Option.unwrapOr[Int](Option.none[Int](), 7), 7));
    Testing.it("'unwrapOrElse' returns fallback for none", Testing.toBe(Option.none[Int]() |> Option.unwrapOrElse[Int](9), 9));
    Testing.it("'fold' maps present values", Testing.toBe(Option.some[Int](2) |> Option.fold[Int, Int](0, \(x) => x + 1), 3));
    Testing.endDescribe()
  );
