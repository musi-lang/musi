let time := import "@std/datetime";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("time/index");
    testing.it(
      "'nowUnix' returns current epoch millis",
      time.nowUnix().toMillis() >= 0,
    );
    testing.it("'nowUnix' derives seconds", time.nowUnix().toSeconds() >= 0,);
    testing.it(
      "'monotonic' returns non-negative value",
      time.monotonic().toMillis() >= 0,
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
