let base64 := import "@std/encoding/base64";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("encoding/base64");
    testing.it(
      "'encode' converts string to base64",
      base64.encode("hi") = "aGk=",
    );
    testing.it("'isValid' accepts base64 string", base64.isValid("aGk="),);
    testing.it(
      "'decode' returns Some for valid input",
      base64.decode("aGk=").isSome(),
    );
    testing.it(
      "'decode' returns None for invalid input",
      base64.decode("bad").isNone(),
    );
    testing.endDescribe()
  );
