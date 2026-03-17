//! Tests for string splitting utilities.

import "@std/testing" as t;
import "@std/text/split" as ts;

t.runner.report(t.runner.run_suite([
  ("lines - splits on newline", () => (
    let result := ts.lines("a\nb\nc");
    t.assertions.assert_eq(result, ["a", "b", "c"])
  )),
  ("lines - single line with no newline", () => (
    let result := ts.lines("hello");
    t.assertions.assert_eq(result, ["hello"])
  )),
  ("lines - empty string gives one empty element", () => (
    let result := ts.lines("");
    t.assertions.assert_eq(result, [""])
  )),
  ("lines - trailing newline produces empty last element", () => (
    let result := ts.lines("a\nb\n");
    t.assertions.assert_eq(result, ["a", "b", ""])
  )),
  ("words - splits on whitespace", () => (
    let result := ts.words("hello world foo");
    t.assertions.assert_eq(result, ["hello", "world", "foo"])
  )),
  ("words - leading and trailing spaces trimmed", () => (
    let result := ts.words("  hello  world  ");
    t.assertions.assert_eq(result, ["hello", "world"])
  )),
  ("words - multiple spaces between words collapsed", () => (
    let result := ts.words("a  b  c");
    t.assertions.assert_eq(result, ["a", "b", "c"])
  )),
  ("words - single word", () => (
    let result := ts.words("hello");
    t.assertions.assert_eq(result, ["hello"])
  )),
  ("words - empty string returns empty array", () => (
    let result := ts.words("");
    t.assertions.assert_eq(result, [])
  )),
  ("words - whitespace-only string returns empty array", () => (
    let result := ts.words("   ");
    t.assertions.assert_eq(result, [])
  )),
]));
