let text := import "./index.ms";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("text/index");
    testing.it("'length' returns character count",
      testing.toBe("trim".length(), 4)
    );
    testing.it("'isEmpty' detects empty string",
      testing.toBeTruthy("".isEmpty())
    );
    testing.it("'trim' strips surrounding spaces",
      testing.toBeTruthy("  trim  ".trim() = "trim")
    );
    testing.it("'toLowerCase' lowers letters",
      testing.toBeTruthy("ABC".toLowerCase() = "abc")
    );
    testing.it("'toUpperCase' raises letters",
      testing.toBeTruthy("abc".toUpperCase() = "ABC")
    );
    testing.it("'contains' finds substring",
      testing.toBeTruthy("abcd".contains("bc"))
    );
    testing.it("'startsWith' finds prefix",
      testing.toBeTruthy("abcd".startsWith("ab"))
    );
    testing.it("'endsWith' finds suffix",
      testing.toBeTruthy("abcd".endsWith("cd"))
    );
    testing.endDescribe()
  );
