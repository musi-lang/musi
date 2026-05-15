let ascii := import "@std/ascii";
let assert := import "@std/assert";
let maybe := import "@std/maybe";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("ascii");
    testing.it("'isAscii' accepts byte range", ascii.isAscii(127));
    testing.it(
      "'isAscii' rejects outside byte range",
      assert.toBeFalse(ascii.isAscii(128)),
    );
    testing.it("'isControl' detects delete", ascii.isControl(127));
    testing.it("'isPrintable' detects letters", ascii.isPrintable(65));
    testing.it("'isWhitespace' detects tab", ascii.isWhitespace(9));
    testing.it("'isWhitespace' detects form feed", ascii.isWhitespace(12));
    testing.it("'isDigit' detects decimal digits", ascii.isDigit(57));
    testing.it("'isAlpha' detects letters", ascii.isAlpha(65));
    testing.it("'isAlphanumeric' detects digits", ascii.isAlphanumeric(48));
    testing.it("'isHexDigit' detects uppercase hex", ascii.isHexDigit(70));
    testing.it(
      "'toLower' lowers uppercase letters",
      assert.toBe(ascii.toLower(65), 97),
    );
    testing.it(
      "'toUpper' raises lowercase letters",
      assert.toBe(ascii.toUpper(97), 65),
    );
    testing.it(
      "'digitValue' returns decimal value",
      assert.toBe(maybe.unwrapOr[Int](ascii.digitValue(57), - 1), 9),
    );
    testing.it(
      "'hexValue' returns hex value",
      assert.toBe(maybe.unwrapOr[Int](ascii.hexValue(70), - 1), 15),
    );
    testing.it(
      "'hexValue' rejects non-hex",
      maybe.isNone[Int](ascii.hexValue(71)),
    );
    testing.endDescribe()
  );
