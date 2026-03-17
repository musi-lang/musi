//! Tests for rune (Unicode character) operations.

import "@std/testing" as t;
import "@std/rune/index" as m;

t.runner.report(t.runner.run_suite([
  ("is_alpha - letter", () => (
    t.assertions.assert_true(m.is_alpha('a'))
  )),
  ("is_alpha - digit", () => (
    t.assertions.assert_false(m.is_alpha('5'))
  )),
  ("is_digit - digit", () => (
    t.assertions.assert_true(m.is_digit('7'))
  )),
  ("is_digit - letter", () => (
    t.assertions.assert_false(m.is_digit('z'))
  )),
  ("is_whitespace - space", () => (
    t.assertions.assert_true(m.is_whitespace(' '))
  )),
  ("is_whitespace - letter", () => (
    t.assertions.assert_false(m.is_whitespace('a'))
  )),
  ("is_alphanumeric - letter", () => (
    t.assertions.assert_true(m.is_alphanumeric('b'))
  )),
  ("is_alphanumeric - digit", () => (
    t.assertions.assert_true(m.is_alphanumeric('3'))
  )),
  ("is_alphanumeric - space", () => (
    t.assertions.assert_false(m.is_alphanumeric(' '))
  )),
  ("is_upper - uppercase letter", () => (
    t.assertions.assert_true(m.is_upper('A'))
  )),
  ("is_upper - lowercase letter", () => (
    t.assertions.assert_false(m.is_upper('a'))
  )),
  ("is_lower - lowercase letter", () => (
    t.assertions.assert_true(m.is_lower('z'))
  )),
  ("is_lower - uppercase letter", () => (
    t.assertions.assert_false(m.is_lower('Z'))
  )),
  ("to_upper - lowercase to uppercase", () => (
    t.assertions.assert_eq(m.to_upper('a'), 'A')
  )),
  ("to_upper - already uppercase", () => (
    t.assertions.assert_eq(m.to_upper('A'), 'A')
  )),
  ("to_lower - uppercase to lowercase", () => (
    t.assertions.assert_eq(m.to_lower('A'), 'a')
  )),
  ("to_lower - already lowercase", () => (
    t.assertions.assert_eq(m.to_lower('a'), 'a')
  )),
  ("to_int and from_int - round trip", () => (
    t.assertions.assert_eq(m.from_int(m.to_int('A')), 'A')
  )),
  ("is_ascii - ASCII char", () => (
    t.assertions.assert_true(m.is_ascii('a'))
  )),
  ("is_hex_digit - hex letter", () => (
    t.assertions.assert_true(m.is_hex_digit('f'))
  )),
  ("is_hex_digit - uppercase hex", () => (
    t.assertions.assert_true(m.is_hex_digit('F'))
  )),
  ("is_hex_digit - digit", () => (
    t.assertions.assert_true(m.is_hex_digit('9'))
  )),
  ("is_hex_digit - non-hex letter", () => (
    t.assertions.assert_false(m.is_hex_digit('g'))
  )),
  ("is_bin_digit - zero", () => (
    t.assertions.assert_true(m.is_bin_digit('0'))
  )),
  ("is_bin_digit - one", () => (
    t.assertions.assert_true(m.is_bin_digit('1'))
  )),
  ("is_bin_digit - two", () => (
    t.assertions.assert_false(m.is_bin_digit('2'))
  )),
  ("is_oct_digit - valid", () => (
    t.assertions.assert_true(m.is_oct_digit('7'))
  )),
  ("is_oct_digit - eight", () => (
    t.assertions.assert_false(m.is_oct_digit('8'))
  )),
]));
