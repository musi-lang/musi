let assert := import "@std/assert";
let expect := import "@std/expect";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("expect");
    let successValue := expect.Success[Int, String](1);
    let failureValue := expect.Failure[Int, String]("boom");
    testing.it("'isSuccess' detects success value", successValue.isSuccess());
    testing.it("'isFailure' detects failure value", failureValue.isFailure(),);
    testing.it(
      "'unwrapOr' returns fallback for failure",
      assert.toBe(failureValue.unwrapOr(9), 9),
    );
    testing.it(
      "'mapOrElse' handles success value",
      assert.toBe(
        expect.mapOrElse[Int, String, Int](
          expect.Success[Int, String](2),
          \(error) => 0,
          \(x) => x + 1,
        ),
        3,
      ),
    );
    testing.it(
      "'fold' handles failure value",
      assert.toBe(
        expect.fold[Int, String, Int](
          failureValue,
          \(value) => value,
          \(error) => 7,
        ),
        7,
      ),
    );
    testing.endDescribe()
  );
