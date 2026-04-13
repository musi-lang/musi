let Testing := import "@std/testing";
let Result := import "@std/result";

export let test () :=
  (
    Testing.describe("result");
    Testing.it("'isOk' detects ok value", Testing.toBeTruthy(Result.isOk[Int, String](Result.ok[Int, String](1))));
    Testing.it("'isErr' detects err value", Testing.toBeTruthy(Result.isErr[Int, String](Result.err[Int, String]("boom"))));
    Testing.it("'unwrapOr' returns fallback for err", Testing.toBe(Result.unwrapOr[Int, String](Result.err[Int, String]("boom"), 9), 9));
    Testing.endDescribe()
  );
