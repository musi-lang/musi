//! Tests for FNV-1a hash functions.

import "@std/testing" as t;
import "@std/hash/index" as m;

t.runner.report(t.runner.run_suite([
  ("hash_int — returns non-negative", () => (
    t.assertions.assert_true(m.hash_int(42) >= 0)
  )),
  ("hash_int — zero input", () => (
    t.assertions.assert_true(m.hash_int(0) >= 0)
  )),
  ("hash_int — negative input", () => (
    t.assertions.assert_true(m.hash_int(-99) >= 0)
  )),
  ("hash_int — same input same output", () => (
    t.assertions.assert_eq(m.hash_int(7), m.hash_int(7))
  )),
  ("hash_int — different inputs differ", () => (
    t.assertions.assert_ne(m.hash_int(1), m.hash_int(2))
  )),
  ("hash_str — returns non-negative", () => (
    t.assertions.assert_true(m.hash_str("hello") >= 0)
  )),
  ("hash_str — empty string", () => (
    t.assertions.assert_true(m.hash_str("") >= 0)
  )),
  ("hash_str — same input same output", () => (
    t.assertions.assert_eq(m.hash_str("abc"), m.hash_str("abc"))
  )),
  ("hash_str — different strings differ", () => (
    t.assertions.assert_ne(m.hash_str("abc"), m.hash_str("xyz"))
  )),
  ("hash_bool — true and false differ", () => (
    t.assertions.assert_ne(m.hash_bool(.True), m.hash_bool(.False))
  )),
  ("hash_bool — stable for true", () => (
    t.assertions.assert_eq(m.hash_bool(.True), m.hash_bool(.True))
  )),
  ("combine — returns non-negative", () => (
    t.assertions.assert_true(m.combine(m.hash_int(1), m.hash_int(2)) >= 0)
  )),
  ("combine — same inputs same output", () => (
    let h1 := m.hash_int(10);
    let h2 := m.hash_int(20);
    t.assertions.assert_eq(m.combine(h1, h2), m.combine(h1, h2))
  )),
]));
