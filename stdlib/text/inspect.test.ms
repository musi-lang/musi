//! Tests for string inspection predicates.

import "@std/testing" as t;
import "@std/text/inspect" as m;

t.runner.report(t.runner.run_suite([
  ("is_blank — empty string", () => (
    t.assertions.assert_true(m.is_blank(""))
  )),
  ("is_blank — whitespace only", () => (
    t.assertions.assert_true(m.is_blank("   "))
  )),
  ("is_blank — non-whitespace", () => (
    t.assertions.assert_false(m.is_blank("hello"))
  )),
  ("is_blank — mixed whitespace and text", () => (
    t.assertions.assert_false(m.is_blank("  hi  "))
  )),
  ("is_alpha — letters only", () => (
    t.assertions.assert_true(m.is_alpha("abc"))
  )),
  ("is_alpha — empty string", () => (
    t.assertions.assert_false(m.is_alpha(""))
  )),
  ("is_alpha — contains digit", () => (
    t.assertions.assert_false(m.is_alpha("abc1"))
  )),
  ("is_alpha — single letter", () => (
    t.assertions.assert_true(m.is_alpha("z"))
  )),
  ("is_digit — digits only", () => (
    t.assertions.assert_true(m.is_digit("123"))
  )),
  ("is_digit — empty string", () => (
    t.assertions.assert_false(m.is_digit(""))
  )),
  ("is_digit — contains letter", () => (
    t.assertions.assert_false(m.is_digit("12a"))
  )),
  ("is_alphanumeric — letters and digits", () => (
    t.assertions.assert_true(m.is_alphanumeric("abc123"))
  )),
  ("is_alphanumeric — empty string", () => (
    t.assertions.assert_false(m.is_alphanumeric(""))
  )),
  ("is_alphanumeric — contains space", () => (
    t.assertions.assert_false(m.is_alphanumeric("abc 123"))
  )),
  ("is_alphanumeric — letters only", () => (
    t.assertions.assert_true(m.is_alphanumeric("hello"))
  )),
]));
