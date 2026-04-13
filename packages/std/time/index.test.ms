let Testing := import "@std/testing";
let Time := import "./index.ms";

export let test () :=
  (
    Testing.describe("time/index");
    Testing.it("'nowUnixMillis' returns current epoch millis", Testing.toBeTruthy(Time.nowUnixMillis() >= 0));
    Testing.it("'nowUnixSeconds' derives seconds", Testing.toBeTruthy(Time.nowUnixSeconds() >= 0));
    Testing.it("'monotonicMillis' returns non-negative value", Testing.toBeTruthy(Time.monotonicMillis() >= 0));
    Testing.endDescribe()
  );
