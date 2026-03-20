//! Tests for rune (Unicode character) operations.

import "@std/testing" as t;
import "@std/rune/index" as m;

t.describe("Rune", () => (
  t.it("is_alpha - letter", () => (
    t.expect(m.is_alpha('a')).toBe(.True)
  ));
  t.it("is_alpha - digit", () => (
    t.expect(m.is_alpha('5')).toBe(.False)
  ));
  t.it("is_digit - digit", () => (
    t.expect(m.is_digit('7')).toBe(.True)
  ));
  t.it("is_digit - letter", () => (
    t.expect(m.is_digit('z')).toBe(.False)
  ));
  t.it("is_whitespace - space", () => (
    t.expect(m.is_whitespace(' ')).toBe(.True)
  ));
  t.it("is_whitespace - letter", () => (
    t.expect(m.is_whitespace('a')).toBe(.False)
  ));
  t.it("is_alphanumeric - letter", () => (
    t.expect(m.is_alphanumeric('b')).toBe(.True)
  ));
  t.it("is_alphanumeric - digit", () => (
    t.expect(m.is_alphanumeric('3')).toBe(.True)
  ));
  t.it("is_alphanumeric - space", () => (
    t.expect(m.is_alphanumeric(' ')).toBe(.False)
  ));
  t.it("is_upper - uppercase letter", () => (
    t.expect(m.is_upper('A')).toBe(.True)
  ));
  t.it("is_upper - lowercase letter", () => (
    t.expect(m.is_upper('a')).toBe(.False)
  ));
  t.it("is_lower - lowercase letter", () => (
    t.expect(m.is_lower('z')).toBe(.True)
  ));
  t.it("is_lower - uppercase letter", () => (
    t.expect(m.is_lower('Z')).toBe(.False)
  ));
  t.it("to_upper - lowercase to uppercase", () => (
    t.expect(m.to_upper('a')).toBe('A')
  ));
  t.it("to_upper - already uppercase", () => (
    t.expect(m.to_upper('A')).toBe('A')
  ));
  t.it("to_lower - uppercase to lowercase", () => (
    t.expect(m.to_lower('A')).toBe('a')
  ));
  t.it("to_lower - already lowercase", () => (
    t.expect(m.to_lower('a')).toBe('a')
  ));
  t.it("to_int and from_int - round trip", () => (
    t.expect(m.from_int(m.to_int('A'))).toBe('A')
  ));
  t.it("is_ascii - ASCII char", () => (
    t.expect(m.is_ascii('a')).toBe(.True)
  ));
  t.it("is_hex_digit - hex letter", () => (
    t.expect(m.is_hex_digit('f')).toBe(.True)
  ));
  t.it("is_hex_digit - uppercase hex", () => (
    t.expect(m.is_hex_digit('F')).toBe(.True)
  ));
  t.it("is_hex_digit - digit", () => (
    t.expect(m.is_hex_digit('9')).toBe(.True)
  ));
  t.it("is_hex_digit - non-hex letter", () => (
    t.expect(m.is_hex_digit('g')).toBe(.False)
  ));
  t.it("is_bin_digit - zero", () => (
    t.expect(m.is_bin_digit('0')).toBe(.True)
  ));
  t.it("is_bin_digit - one", () => (
    t.expect(m.is_bin_digit('1')).toBe(.True)
  ));
  t.it("is_bin_digit - two", () => (
    t.expect(m.is_bin_digit('2')).toBe(.False)
  ));
  t.it("is_oct_digit - valid", () => (
    t.expect(m.is_oct_digit('7')).toBe(.True)
  ));
  t.it("is_oct_digit - eight", () => (
    t.expect(m.is_oct_digit('8')).toBe(.False)
  ))
));

t.run();
