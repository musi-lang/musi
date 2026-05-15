let utf8 := import "@std/encoding/utf8";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("encoding/utf8");
    testing.it("'encode' keeps ordinary utf8 string", utf8.encode("hi") = "hi",);
    testing.it("'isValid' accepts valid utf8 string", utf8.isValid("hi"),);
    testing.it(
      "'decode' returns Some for valid input",
      utf8.decode("hi").isSome(),
    );
    testing.endDescribe()
  );
