let hex := import "@std/encoding/hex";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("encoding/hex");
    testing.it(
      "'encode' converts utf8 bytes to hex",
      hex.encode("hi") = "6869",
    );
    testing.it("'isValid' accepts hex string", hex.isValid("6869"),);
    testing.it(
      "'decode' returns some for valid input",
      hex.decode("6869").isSome(),
    );
    testing.it(
      "'decode' returns decoded text",
      hex.decode("6869").unwrapOr("") = "hi",
    );
    testing.it(
      "'decode' returns none for invalid input",
      hex.decode("zz").isNone(),
    );
    testing.endDescribe()
  );
