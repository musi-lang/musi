let random := import "@std/random";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("random");
    testing.it(
      "'nextIntInRange' stays above lower bound",
      random.nextIntInRange(1, 10) >= 1,
    );
    testing.it(
      "'nextIntInRange' stays below upper bound",
      random.nextIntInRange(1, 10) < 10,
    );
    testing.it("'nextFloat' stays above zero", random.nextFloat() >= 0.0,);
    testing.it("'nextFloat' stays below one", random.nextFloat() <= 1.0,);
    testing.it(
      "'chance' always fails below zero",
      testing.toBeFalse(random.chance(- 1)),
    );
    testing.it("'chance' always passes above one hundred", random.chance(101),);
    testing.endDescribe()
  );
