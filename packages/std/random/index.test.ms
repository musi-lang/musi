let Testing := import "@std/testing";
let Random := import "./index.ms";

export let test () :=
  (
    Testing.describe("random/index");
    Testing.it("'intInRange' stays above lower bound", Testing.toBeTruthy(Random.intInRange(1, 10) >= 1));
    Testing.it("'intInRange' stays below upper bound", Testing.toBeTruthy(Random.intInRange(1, 10) < 10));
    Testing.it("'float01' stays above zero", Testing.toBeTruthy(Random.float01() >= 0.0));
    Testing.it("'float01' stays below one", Testing.toBeTruthy(Random.float01() <= 1.0));
    Testing.it("'chance' always fails below zero", Testing.toBeFalsy(Random.chance(-1)));
    Testing.it("'chance' always passes above one hundred", Testing.toBeTruthy(Random.chance(101)));
    Testing.endDescribe()
  );
