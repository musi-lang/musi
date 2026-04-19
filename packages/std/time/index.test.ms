let time := import "./index.ms";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("time/index");
    testing.it(
      "'nowUnix' returns current epoch millis",
      testing.toBeTruthy(time.nowUnix().toMillis() >= 0),
    );
    testing.it(
      "'nowUnix' derives seconds",
      testing.toBeTruthy(time.nowUnix().toSeconds() >= 0),
    );
    testing.it(
      "'monotonic' returns non-negative value",
      testing.toBeTruthy(time.monotonic().toMillis() >= 0),
    );
    testing.it(
      "'seconds' converts to millis",
      testing.toBe(time.seconds(2).toMillis(), 2000),
    );
    testing.it(
      "'minutes' converts to seconds",
      testing.toBe(time.minutes(2).toSeconds(), 120),
    );
    testing.endDescribe()
  );
