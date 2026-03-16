//! Tests for string join utilities.

import "@std/testing" as t;
import "@std/text/join" as tj;

t.runner.report(t.runner.run_suite([
  ("join — joins with separator", () => (
    let result := tj.join(["a", "b", "c"], ", ");
    t.assertions.assert_eq(result, "a, b, c")
  )),
  ("join — single element no separator", () => (
    let result := tj.join(["hello"], ", ");
    t.assertions.assert_eq(result, "hello")
  )),
  ("join — empty array returns empty string", () => (
    let result := tj.join([], ", ");
    t.assertions.assert_eq(result, "")
  )),
  ("join — empty separator concatenates", () => (
    let result := tj.join(["a", "b", "c"], "");
    t.assertions.assert_eq(result, "abc")
  )),
  ("join — newline separator", () => (
    let result := tj.join(["line1", "line2", "line3"], "\n");
    t.assertions.assert_eq(result, "line1\nline2\nline3")
  )),
  ("join — multi-char separator", () => (
    let result := tj.join(["x", "y"], " | ");
    t.assertions.assert_eq(result, "x | y")
  )),
  ("join — two elements", () => (
    let result := tj.join(["foo", "bar"], "-");
    t.assertions.assert_eq(result, "foo-bar")
  )),
]));
