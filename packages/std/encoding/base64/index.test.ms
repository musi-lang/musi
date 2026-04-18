let base64 := import "./index.ms";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("encoding/base64");
    testing.it("'encode' converts string to base64",
      testing.toBeTruthy(base64.encode("hi") = "aGk=")
    );
    testing.it("'isValid' accepts base64 string",
      testing.toBeTruthy(base64.isValid("aGk="))
    );
    testing.it("'decode' returns some for valid input",
      testing.toBeTruthy(base64.decode("aGk=").isSome())
    );
    testing.it("'decode' returns none for invalid input",
      testing.toBeTruthy(base64.decode("bad").isNone())
    );
    testing.endDescribe()
  );
