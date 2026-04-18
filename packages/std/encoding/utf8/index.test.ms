let utf8 := import "./index.ms";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("encoding/utf8");
    testing.it("'encode' keeps ordinary utf8 string",
      testing.toBeTruthy(utf8.encode("hi") = "hi")
    );
    testing.it("'isValid' accepts valid utf8 string",
      testing.toBeTruthy(utf8.isValid("hi"))
    );
    testing.it("'decode' returns some for valid input",
      testing.toBeTruthy(utf8.decode("hi").isSome())
    );
    testing.endDescribe()
  );
