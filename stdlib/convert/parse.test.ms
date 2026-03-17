//! Tests for safe parse conversion functions.

import "@std/testing" as t;
import "@std/convert/parse" as m;

t.runner.report(t.runner.run_suite([
  ("parse_int - valid integer string", () => (
    t.assertions.assert_eq(m.parse_int("42"), .Some(42))
  )),
  ("parse_int - zero string", () => (
    t.assertions.assert_eq(m.parse_int("0"), .Some(0))
  )),
  ("parse_int - negative string", () => (
    t.assertions.assert_eq(m.parse_int("-5"), .Some(-5))
  )),
  ("parse_int - non-integer string", () => (
    t.assertions.assert_eq(m.parse_int("abc"), .None)
  )),
  ("parse_int - float string", () => (
    t.assertions.assert_eq(m.parse_int("3.14"), .None)
  )),
  ("parse_float - valid float string", () => (
    t.assertions.assert_eq(m.parse_float("3.14"), .Some(3.14))
  )),
  ("parse_float - integer string", () => (
    t.assertions.assert_eq(m.parse_float("2"), .Some(2.0))
  )),
  ("parse_float - non-numeric string", () => (
    t.assertions.assert_eq(m.parse_float("xyz"), .None)
  )),
  ("digit_to_int - digit zero", () => (
    t.assertions.assert_eq(m.digit_to_int('0'), .Some(0))
  )),
  ("digit_to_int - digit nine", () => (
    t.assertions.assert_eq(m.digit_to_int('9'), .Some(9))
  )),
  ("digit_to_int - letter", () => (
    t.assertions.assert_eq(m.digit_to_int('a'), .None)
  )),
  ("int_to_digit - zero", () => (
    t.assertions.assert_eq(m.int_to_digit(0), .Some('0'))
  )),
  ("int_to_digit - nine", () => (
    t.assertions.assert_eq(m.int_to_digit(9), .Some('9'))
  )),
  ("int_to_digit - out of range", () => (
    t.assertions.assert_eq(m.int_to_digit(10), .None)
  )),
  ("int_to_digit - negative", () => (
    t.assertions.assert_eq(m.int_to_digit(-1), .None)
  )),
]));
