let Testing := import "@std/testing";
let Result := import "@std/result";

export let test () :=
    (
        Testing.describe("result");
        Testing.it("is_ok detects ok value", Testing.to_be_truthy(Result.is_ok[Int, String](Result.ok[Int, String](1))));
        Testing.it("is_err detects err value", Testing.to_be_truthy(Result.is_err[Int, String](Result.err[Int, String]("boom"))));
        Testing.it("unwrap_or returns fallback for err", Testing.to_be(Result.unwrap_or[Int, String](Result.err[Int, String]("boom"), 9), 9));
        Testing.end_describe()
    );
