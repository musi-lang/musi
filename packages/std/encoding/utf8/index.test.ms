let Testing := import "@std/testing";
let Option := import "@std/option";
let Utf8 := import "./index.ms";

export let test () :=
  (
    Testing.describe("encoding/utf8");
    Testing.it("'encode' keeps ordinary utf8 string", Testing.toBeTruthy(Utf8.encode("hi") = "hi"));
    Testing.it("'isValid' accepts valid utf8 string", Testing.toBeTruthy(Utf8.isValid("hi")));
    Testing.it("'decode' returns some for valid input", Testing.toBeTruthy(Option.isSome[String](Utf8.decode("hi"))));
    Testing.endDescribe()
  );
