let testing := import "@std/testing";
let text := import "@std/text";

export let test () :=
  (
    testing.describe("text/index");
    testing.it(
      "'length' returns character count",
      testing.toBe("trim".length(), 4),
    );
    testing.it("'isEmpty' detects empty string", "".isEmpty(),);
    testing.it("'trim' strips surrounding spaces", "  trim  ".trim() = "trim",);
    testing.it(
      "'concat' joins strings",
      text.concat("hello", " world") = "hello world",
    );
    testing.it("'fromByte' builds ascii text", text.fromByte(65) = "A",);
    testing.it("'toLowerCase' lowers letters", "ABC".toLowerCase() = "abc",);
    testing.it("'toUpperCase' raises letters", "abc".toUpperCase() = "ABC",);
    testing.it("'contains' finds substring", "abcd".contains("bc"),);
    testing.it("'startsWith' finds prefix", "abcd".startsWith("ab"),);
    testing.it("'endsWith' finds suffix", "abcd".endsWith("cd"),);
    testing.endDescribe()
  );
