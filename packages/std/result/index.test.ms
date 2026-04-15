let Testing := import "@std/testing";
let Result := import "@std/result";

export let test () :=
  (
    Testing.describe("result");
    Testing.it("'isOk' detects ok value", Testing.toBeTruthy(Result.isOk[Int, String](Result.ok[Int, String](1))));
    Testing.it("'isErr' detects err value", Testing.toBeTruthy(Result.isErr[Int, String](Result.err[Int, String]("boom"))));
    Testing.it("'unwrapOr' returns fallback for err", Testing.toBe(Result.unwrapOr[Int, String](Result.err[Int, String]("boom"), 9), 9));
    Testing.it("'mapOrElse' handles ok value", Testing.toBe(Result.ok[Int, String](2) |> Result.mapOrElse[Int, String, Int](\(error) => 0, \(x) => x + 1), 3));
    Testing.it("'fold' handles err value", Testing.toBe(Result.err[Int, String]("boom") |> Result.fold[Int, String, Int](\(value) => value, \(error) => 7), 7));
    Testing.endDescribe()
  );
