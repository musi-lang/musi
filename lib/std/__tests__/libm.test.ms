let libm := import "@std/libm";
let maybe := import "@std/maybe";
let testing := import "@std/testing";

let close (actual : Float, expected : Float) : Bit :=
  libm.fabs(actual - expected) < 0.000001;

export let test () :=
  (
    testing.describe("libm");
    testing.it("'sqrt' returns square root", close(libm.sqrt(9.0), 3.0));
    testing.it("'pow' returns power", close(libm.pow(2.0, 5.0), 32.0));
    testing.it("'sin' handles zero", close(libm.sin(0.0), 0.0));
    testing.it("'cos' handles zero", close(libm.cos(0.0), 1.0));
    testing.it("'floor' rounds down", close(libm.floor(2.75), 2.0));
    testing.it("'ceil' rounds up", close(libm.ceil(2.25), 3.0));
    let nan := libm.sqrt(- 1.0);
    let infinity := libm.log(0.0 - 0.0);
    testing.it("'isNan' detects NaN", libm.isNan(nan));
    testing.it("'isFinite' rejects NaN", libm.isFinite(nan) = (0 = 1));
    testing.it("'isInfinite' detects infinity", libm.isInfinite(infinity));
    testing.it("'isFinite' accepts finite values", libm.isFinite(1.0));
    testing.it(
      "'checkedSqrt' accepts non-negative input",
      maybe.isSome[Float](libm.checkedSqrt(9.0)),
    );
    testing.it(
      "'checkedSqrt' rejects negative input",
      maybe.isNone[Float](libm.checkedSqrt(- 1.0)),
    );
    testing.it(
      "'checkedSqrt' rejects NaN",
      maybe.isNone[Float](libm.checkedSqrt(nan)),
    );
    testing.it(
      "'checkedLog' rejects zero",
      maybe.isNone[Float](libm.checkedLog(0.0)),
    );
    testing.it(
      "'checkedLog' rejects infinity",
      maybe.isNone[Float](libm.checkedLog(infinity)),
    );
    testing.it(
      "'checkedAcos' rejects out-of-domain input",
      maybe.isNone[Float](libm.checkedAcos(2.0)),
    );
    testing.it(
      "'checkedAcos' rejects NaN",
      maybe.isNone[Float](libm.checkedAcos(nan)),
    );
    testing.it(
      "'checkedFmod' rejects zero divisor",
      maybe.isNone[Float](libm.checkedFmod(2.0, 0.0)),
    );
    testing.it(
      "'checkedFmod' rejects NaN dividend",
      maybe.isNone[Float](libm.checkedFmod(nan, 2.0)),
    );
    testing.endDescribe()
  );
