let encoding := import "@std/encoding";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("encoding");
    testing.it("hex namespace is exported", encoding.hex.encode("hi") = "6869",);
    testing.it(
      "base64 namespace is exported",
      encoding.base64.encode("hi") = "aGk=",
    );
    testing.it("utf8 namespace is exported", encoding.utf8.isValid("hi"));
    testing.endDescribe()
  );
