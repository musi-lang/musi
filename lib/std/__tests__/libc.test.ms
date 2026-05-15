let assert := import "@std/assert";
let libc := import "@std/libc";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("libc");
    testing.it("'strlen' counts C string bytes", libc.strlen("musi") > 0);
    testing.it(
      "'strcmp' matches equal C strings",
      libc.strcmp("musi", "musi") = libc.strcmp("std", "std"),
    );
    testing.it("'strerror' returns text", libc.strlen(libc.strerror(2)) > 0);
    testing.it(
      "'length' mirrors strlen",
      libc.length("musi") = libc.strlen("musi"),
    );
    testing.it("'equals' accepts equal text", libc.equals("musi", "musi"));
    testing.it(
      "'compare' reports equal text",
      libc.compare("musi", "musi").isEqual(),
    );
    testing.it(
      "'errorMessage' returns text",
      libc.length(libc.errorMessage(2)) > 0,
    );
    testing.it("'isAlpha' classifies ASCII letters", libc.isAlpha(65));
    testing.it("'isDigit' classifies ASCII digits", libc.isDigit(57));
    testing.it("'isXDigit' classifies ASCII hex", libc.isXDigit(70));
    testing.it("'isSpace' classifies ASCII whitespace", libc.isSpace(10));
    testing.it(
      "'toLower' lowers ASCII letters",
      assert.toBe(libc.toLower(65), 97),
    );
    testing.it(
      "'toUpper' raises ASCII letters",
      assert.toBe(libc.toUpper(97), 65),
    );
    testing.endDescribe()
  );
