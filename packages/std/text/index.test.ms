let testing := import "@std/testing";
let text := import "./index.ms";

export let test () :=
  (
    testing.describe("text/index");
    testing.it("'length' returns character count", testing.toBe(text.length("trim"), 4));
    testing.it("'isEmpty' detects empty string", testing.toBeTruthy(text.isEmpty("")));
    testing.it("'trim' strips surrounding spaces", testing.toBeTruthy(text.trim("  trim  ") = "trim"));
    testing.it("'toLowerCase' lowers letters", testing.toBeTruthy(text.toLowerCase("ABC") = "abc"));
    testing.it("'toUpperCase' raises letters", testing.toBeTruthy(text.toUpperCase("abc") = "ABC"));
    testing.it("'contains' finds substring", testing.toBeTruthy(text.contains("abcd", "bc")));
    testing.it("'startsWith' finds prefix", testing.toBeTruthy(text.startsWith("abcd", "ab")));
    testing.it("'endsWith' finds suffix", testing.toBeTruthy(text.endsWith("abcd", "cd")));
    testing.endDescribe()
  );
