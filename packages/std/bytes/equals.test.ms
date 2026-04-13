let Testing := import "@std/testing";
let Bytes := import "@std/bytes";

export let test () :=
  (
    Testing.describe("bytes/equals");
    Testing.it(
      "returns true for equal arrays",
      Testing.toBeTruthy(Bytes.equals([1, 2, 3], [1, 2, 3])),
    );
    Testing.it(
      "returns false for different arrays",
      Testing.toBeFalsy(Bytes.equals([1, 2, 3], [1, 2, 4])),
    );
    Testing.endDescribe()
  );
