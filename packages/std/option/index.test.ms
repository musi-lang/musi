let Testing := import "@std/testing";
let Option := import "@std/option";

export let test () :=
  (
    Testing.describe("option");
    Testing.it("'isSome' detects present value", Testing.toBeTruthy(Option.isSome[Int](Option.some[Int](1))));
    Testing.it("'isNone' detects missing value", Testing.toBeTruthy(Option.isNone[Int](Option.none[Int]())));
    Testing.it("'unwrapOr' returns fallback for none", Testing.toBe(Option.unwrapOr[Int](Option.none[Int](), 7), 7));
    Testing.endDescribe()
  );
