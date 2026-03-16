//! Tests for string search utilities.

import "@std/testing" as t;
import "@std/text/search" as ts;

t.runner.report(t.runner.run_suite([
  ("contains — substring present returns true", () => (
    let result := ts.contains("hello world", "world");
    t.assertions.assert_true(result)
  )),
  ("contains — substring absent returns false", () => (
    let result := ts.contains("hello world", "xyz");
    t.assertions.assert_false(result)
  )),
  ("contains — empty substring returns true", () => (
    let result := ts.contains("hello", "");
    t.assertions.assert_true(result)
  )),
  ("contains — match at start", () => (
    let result := ts.contains("hello", "hel");
    t.assertions.assert_true(result)
  )),
  ("contains — match at end", () => (
    let result := ts.contains("hello", "llo");
    t.assertions.assert_true(result)
  )),
  ("count_occurrences — counts non-overlapping matches", () => (
    let result := ts.count_occurrences("abababab", "ab");
    t.assertions.assert_eq(result, 4)
  )),
  ("count_occurrences — no matches returns zero", () => (
    let result := ts.count_occurrences("hello", "xyz");
    t.assertions.assert_eq(result, 0)
  )),
  ("count_occurrences — empty substring returns zero", () => (
    let result := ts.count_occurrences("hello", "");
    t.assertions.assert_eq(result, 0)
  )),
  ("count_occurrences — single match", () => (
    let result := ts.count_occurrences("hello world", "world");
    t.assertions.assert_eq(result, 1)
  )),
  ("last_index_of — finds last occurrence", () => (
    let result := ts.last_index_of("abcabc", "bc");
    t.assertions.assert_eq(result, 4)
  )),
  ("last_index_of — single occurrence", () => (
    let result := ts.last_index_of("hello world", "world");
    t.assertions.assert_eq(result, 6)
  )),
  ("last_index_of — not found returns -1", () => (
    let result := ts.last_index_of("hello", "xyz");
    t.assertions.assert_eq(result, -1)
  )),
  ("last_index_of — empty substring returns -1", () => (
    let result := ts.last_index_of("hello", "");
    t.assertions.assert_eq(result, -1)
  )),
]));
