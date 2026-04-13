let Testing := import "@std/testing";
let Text := import "./index.ms";

export let test () :=
  (
    Testing.describe("text/index");
    Testing.it("'length' returns character count", Testing.toBe(Text.length("trim"), 4));
    Testing.it("'isEmpty' detects empty string", Testing.toBeTruthy(Text.isEmpty("")));
    Testing.it("'trim' strips surrounding spaces", Testing.toBeTruthy(Text.trim("  trim  ") = "trim"));
    Testing.it("'toLowerCase' lowers letters", Testing.toBeTruthy(Text.toLowerCase("ABC") = "abc"));
    Testing.it("'toUpperCase' raises letters", Testing.toBeTruthy(Text.toUpperCase("abc") = "ABC"));
    Testing.it("'contains' finds substring", Testing.toBeTruthy(Text.contains("abcd", "bc")));
    Testing.it("'startsWith' finds prefix", Testing.toBeTruthy(Text.startsWith("abcd", "ab")));
    Testing.it("'endsWith' finds suffix", Testing.toBeTruthy(Text.endsWith("abcd", "cd")));
    Testing.endDescribe()
  );
