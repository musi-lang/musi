let Testing := import "@std/testing";
let Option := import "@std/option";
let Hex := import "./index.ms";

export let test () :=
  (
    Testing.describe("encoding/hex");
    Testing.it("'encode' converts utf8 bytes to hex", Testing.toBeTruthy(Hex.encode("hi") = "6869"));
    Testing.it("'isValid' accepts hex string", Testing.toBeTruthy(Hex.isValid("6869")));
    Testing.it("'decode' returns some for valid input", Testing.toBeTruthy(Option.isSome[String](Hex.decode("6869"))));
    Testing.it("'decode' returns none for invalid input", Testing.toBeTruthy(Option.isNone[String](Hex.decode("zz"))));
    Testing.endDescribe()
  );
