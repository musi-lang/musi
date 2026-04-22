let fmt := import "./index.ms";
let datetime := import "@std/datetime";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("fmt"),
    testing.it("formats ints", testing.toBeTruthy(fmt.int(42) = "42")),
    testing.it("formats bools", testing.toBeTruthy(fmt.bool(0 = 0) = "true")),
    testing.it(
      "formats durations",
      testing.toBeTruthy(fmt.duration(datetime.millis(7)) = "7ms"),
    ),
    testing.endDescribe(),
  );
