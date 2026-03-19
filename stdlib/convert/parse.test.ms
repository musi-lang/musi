//! Tests for safe parse conversion functions.

import "@std/testing" as t;
import "@std/convert/parse" as m;

t.describe("Convert.parse", () => (
  t.it("parse_int - valid integer string", () => (
    t.expect(m.parse_int("42")).toBe(.Some(42))
  ));
  t.it("parse_int - zero string", () => (
    t.expect(m.parse_int("0")).toBe(.Some(0))
  ));
  t.it("parse_int - negative string", () => (
    t.expect(m.parse_int("-5")).toBe(.Some(-5))
  ));
  t.it("parse_int - non-integer string", () => (
    t.expect(m.parse_int("abc")).toBe(.None)
  ));
  t.it("parse_int - float string", () => (
    t.expect(m.parse_int("3.14")).toBe(.None)
  ));
  t.it("parse_float - valid float string", () => (
    t.expect(m.parse_float("3.14")).toBe(.Some(3.14))
  ));
  t.it("parse_float - integer string", () => (
    t.expect(m.parse_float("2")).toBe(.Some(2.0))
  ));
  t.it("parse_float - non-numeric string", () => (
    t.expect(m.parse_float("xyz")).toBe(.None)
  ));
  t.it("digit_to_int - digit zero", () => (
    t.expect(m.digit_to_int('0')).toBe(.Some(0))
  ));
  t.it("digit_to_int - digit nine", () => (
    t.expect(m.digit_to_int('9')).toBe(.Some(9))
  ));
  t.it("digit_to_int - letter", () => (
    t.expect(m.digit_to_int('a')).toBe(.None)
  ));
  t.it("int_to_digit - zero", () => (
    t.expect(m.int_to_digit(0)).toBe(.Some('0'))
  ));
  t.it("int_to_digit - nine", () => (
    t.expect(m.int_to_digit(9)).toBe(.Some('9'))
  ));
  t.it("int_to_digit - out of range", () => (
    t.expect(m.int_to_digit(10)).toBe(.None)
  ));
  t.it("int_to_digit - negative", () => (
    t.expect(m.int_to_digit(-1)).toBe(.None)
  ))
));

t.run();
