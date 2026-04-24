let result := import "@std/result";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("result");
    let okValue := result.ok[Int, String](1);
    let errValue := result.err[Int, String]("boom");
    testing.it("'isOk' detects ok value", okValue.isOk());
    testing.it("'isErr' detects err value", errValue.isErr(),);
    testing.it(
      "'unwrapOr' returns fallback for err",
      testing.toBe(errValue.unwrapOr(9), 9),
    );
    testing.it(
      "'mapOrElse' handles ok value",
      testing.toBe(
        result.mapOrElse[Int, String, Int](
          result.ok[Int, String](2),
          \(error) => 0,
          \(x) => x + 1,
        ),
        3,
      ),
    );
    testing.it(
      "'fold' handles err value",
      testing.toBe(
        result.fold[Int, String, Int](
          errValue,
          \(value) => value,
          \(error) => 7,
        ),
        7,
      ),
    );
    testing.endDescribe()
  );
