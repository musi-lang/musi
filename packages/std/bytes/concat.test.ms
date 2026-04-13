let Testing := import "@std/testing";
let Bytes := import "@std/bytes";

export let test () :=
  (
    Testing.describe("bytes/concat");
    Testing.it(
      "concatenates arrays",
      Testing.toBeTruthy(Bytes.equals(Bytes.concat([1, 2], [3, 4]), [1, 2, 3, 4])),
    );
    Testing.endDescribe()
  );
