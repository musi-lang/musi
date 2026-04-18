let random := import "./index.ms";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("random/index");
    testing.it("'nextIntInRange' stays above lower bound",
      testing.toBeTruthy(random.nextIntInRange(1, 10) >= 1)
    );
    testing.it("'nextIntInRange' stays below upper bound",
      testing.toBeTruthy(random.nextIntInRange(1, 10) < 10)
    );
    testing.it("'nextFloat' stays above zero",
      testing.toBeTruthy(random.nextFloat() >= 0.0)
    );
    testing.it("'nextFloat' stays below one",
      testing.toBeTruthy(random.nextFloat() <= 1.0)
    );
    testing.it("'chance' always fails below zero",
      testing.toBeFalsy(random.chance(- 1))
    );
    testing.it("'chance' always passes above one hundred",
      testing.toBeTruthy(random.chance(101))
    );
    testing.endDescribe()
  );
