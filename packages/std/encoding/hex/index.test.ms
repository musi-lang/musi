let testing := import "@std/testing";
let option := import "@std/option";
let hex := import "./index.ms";

export let test () :=
  (
    testing.describe("encoding/hex");
    testing.it("'encode' converts utf8 bytes to hex", testing.toBeTruthy(hex.encode("hi") = "6869"));
    testing.it("'isValid' accepts hex string", testing.toBeTruthy(hex.isValid("6869")));
    testing.it("'decode' returns some for valid input", testing.toBeTruthy(option.isSome[String](hex.decode("6869"))));
    testing.it("'decode' returns none for invalid input", testing.toBeTruthy(option.isNone[String](hex.decode("zz"))));
    testing.endDescribe()
  );
