//! Tests for string padding utilities.

import "@std/testing" as t;
import "@std/text/pad" as pad;

t.describe("Text.pad", () => (
  t.it("pad_left - pads shorter string", () => (
    let result := pad.pad_left("hi", 5, " ");
    t.expect(result).toBe("   hi")
  ));
  t.it("pad_left - string already at width unchanged", () => (
    let result := pad.pad_left("hello", 5, " ");
    t.expect(result).toBe("hello")
  ));
  t.it("pad_left - string longer than width unchanged", () => (
    let result := pad.pad_left("toolong", 3, " ");
    t.expect(result).toBe("toolong")
  ));
  t.it("pad_left - zero pad char", () => (
    let result := pad.pad_left("42", 5, "0");
    t.expect(result).toBe("00042")
  ));
  t.it("pad_left - empty string pads fully", () => (
    let result := pad.pad_left("", 3, "*");
    t.expect(result).toBe("***")
  ));
  t.it("pad_left - width zero returns string as-is", () => (
    let result := pad.pad_left("hi", 0, " ");
    t.expect(result).toBe("hi")
  ));
  t.it("pad_right - pads shorter string", () => (
    let result := pad.pad_right("hi", 5, " ");
    t.expect(result).toBe("hi   ")
  ));
  t.it("pad_right - string already at width unchanged", () => (
    let result := pad.pad_right("hello", 5, " ");
    t.expect(result).toBe("hello")
  ));
  t.it("pad_right - string longer than width unchanged", () => (
    let result := pad.pad_right("toolong", 3, " ");
    t.expect(result).toBe("toolong")
  ));
  t.it("pad_right - custom pad char", () => (
    let result := pad.pad_right("ok", 5, ".");
    t.expect(result).toBe("ok...")
  ));
  t.it("pad_right - empty string pads fully", () => (
    let result := pad.pad_right("", 4, "-");
    t.expect(result).toBe("----")
  ));
  t.it("pad_right - width zero returns string as-is", () => (
    let result := pad.pad_right("hi", 0, " ");
    t.expect(result).toBe("hi")
  ))
));

t.run();
