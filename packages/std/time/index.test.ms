let testing := import "@std/testing";
let time := import "./index.ms";

export let test () :=
  (
    testing.describe("time/index");
    testing.it("'nowUnixMillis' returns current epoch millis", testing.toBeTruthy(time.nowUnixMillis() >= 0));
    testing.it("'nowUnixSeconds' derives seconds", testing.toBeTruthy(time.nowUnixSeconds() >= 0));
    testing.it("'monotonicMillis' returns non-negative value", testing.toBeTruthy(time.monotonicMillis() >= 0));
    testing.it("'seconds' converts to millis", testing.toBe(time.toMillis(time.seconds(2)), 2000));
    testing.it("'toSeconds' converts millis", testing.toBe(time.toSeconds(time.millis(3000)), 3));
    testing.endDescribe()
  );
