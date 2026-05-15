let datetime := import "@std/datetime";
let fmt := import "@std/fmt";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("fmt"),
    testing.it("formats ints", fmt.int(42) = "42"),
    testing.it("formats bools", fmt.bool(0 = 0) = "true"),
    testing.it("formats durations", fmt.duration(datetime.millis(7)) = "7ms",),
    testing.endDescribe(),
  );
