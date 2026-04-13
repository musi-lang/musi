let Testing := import "@std/testing";
let Bytes := import "@std/bytes";

export let test () :=
  (
    Testing.describe("bytes/copy");
    Testing.it(
      "copies the array",
      Testing.toBeTruthy(Bytes.equals(Bytes.copy([1, 2, 3]), [1, 2, 3])),
    );
    Testing.endDescribe()
  );
