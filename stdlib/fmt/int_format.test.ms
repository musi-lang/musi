//! Tests for integer formatting utilities.

import "@std/testing" as t;
import "@std/fmt/int_format" as m;

t.runner.report(t.runner.run_suite([
  ("int_to_hex - zero", () => (
    t.assertions.assert_eq(m.int_to_hex(0), "0")
  )),
  ("int_to_hex - single digit", () => (
    t.assertions.assert_eq(m.int_to_hex(10), "a")
  )),
  ("int_to_hex - 255", () => (
    t.assertions.assert_eq(m.int_to_hex(255), "ff")
  )),
  ("int_to_hex - 256", () => (
    t.assertions.assert_eq(m.int_to_hex(256), "100")
  )),
  ("int_to_hex - negative", () => (
    t.assertions.assert_eq(m.int_to_hex(-1), "-1")
  )),
  ("int_to_hex - 15 is f", () => (
    t.assertions.assert_eq(m.int_to_hex(15), "f")
  )),
  ("int_to_hex - 16 is 10", () => (
    t.assertions.assert_eq(m.int_to_hex(16), "10")
  )),
  ("int_to_bin - zero", () => (
    t.assertions.assert_eq(m.int_to_bin(0), "0")
  )),
  ("int_to_bin - one", () => (
    t.assertions.assert_eq(m.int_to_bin(1), "1")
  )),
  ("int_to_bin - two", () => (
    t.assertions.assert_eq(m.int_to_bin(2), "10")
  )),
  ("int_to_bin - eight", () => (
    t.assertions.assert_eq(m.int_to_bin(8), "1000")
  )),
  ("int_to_bin - negative", () => (
    t.assertions.assert_eq(m.int_to_bin(-2), "-10")
  )),
  ("int_to_bin - 255", () => (
    t.assertions.assert_eq(m.int_to_bin(255), "11111111")
  )),
]));
