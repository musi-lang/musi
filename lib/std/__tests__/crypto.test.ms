let crypto := import "@std/crypto";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("crypto"),
    testing.it(
      "sha256 hex matches empty string digest",
      crypto.sha256Hex("") = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
    ),
    testing.it("random bytes returns text", crypto.randomBytes(4) /= "",),
    testing.endDescribe(),
  );
