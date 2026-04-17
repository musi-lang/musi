let testing := import "@std/testing";
let time := import "./index.ms";

export let test () :=
  (
    testing.describe("time/index");
    testing.it("'nowUnix' returns current epoch millis", testing.toBeTruthy(time.unixTimeMillis(time.nowUnix()) >= 0));
    testing.it("'nowUnix' derives seconds", testing.toBeTruthy(time.unixTimeSeconds(time.nowUnix()) >= 0));
    testing.it("'monotonic' returns non-negative value", testing.toBeTruthy(time.instantMillis(time.monotonic()) >= 0));
    testing.it("'seconds' converts to millis", testing.toBe(time.durationMillis(time.seconds(2)), 2000));
    testing.it("'minutes' converts to seconds", testing.toBe(time.durationSeconds(time.minutes(2)), 120));
    testing.endDescribe()
  );
