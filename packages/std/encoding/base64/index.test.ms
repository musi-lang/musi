let Testing := import "@std/testing";
let Option := import "@std/option";
let Base64 := import "./index.ms";

export let test () :=
  (
    Testing.describe("encoding/base64");
    Testing.it("'encode' converts string to base64", Testing.toBeTruthy(Base64.encode("hi") = "aGk="));
    Testing.it("'isValid' accepts base64 string", Testing.toBeTruthy(Base64.isValid("aGk=")));
    Testing.it("'decode' returns some for valid input", Testing.toBeTruthy(Option.isSome[String](Base64.decode("aGk="))));
    Testing.it("'decode' returns none for invalid input", Testing.toBeTruthy(Option.isNone[String](Base64.decode("bad"))));
    Testing.endDescribe()
  );
