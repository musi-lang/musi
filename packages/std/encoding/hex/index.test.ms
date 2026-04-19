let hex := import "./index.ms";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("encoding/hex");
    testing.it(
      "'encode' converts utf8 bytes to hex",
      testing.toBeTruthy(hex.encode("hi") = "6869"),
    );
    testing.it(
      "'isValid' accepts hex string",
      testing.toBeTruthy(hex.isValid("6869")),
    );
    testing.it(
      "'decode' returns some for valid input",
      testing.toBeTruthy(hex.decode("6869").isSome()),
    );
    testing.it(
      "'decode' returns none for invalid input",
      testing.toBeTruthy(hex.decode("zz").isNone()),
    );
    testing.endDescribe()
  );
