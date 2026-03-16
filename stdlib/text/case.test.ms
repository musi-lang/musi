//! Tests for string case transformations.

import "@std/testing" as t;
import "@std/text/case" as m;

t.runner.report(t.runner.run_suite([
  ("capitalize — lowercase first letter", () => (
    t.assertions.assert_eq(m.capitalize("hello"), "Hello")
  )),
  ("capitalize — already capitalized", () => (
    t.assertions.assert_eq(m.capitalize("Hello"), "Hello")
  )),
  ("capitalize — empty string", () => (
    t.assertions.assert_eq(m.capitalize(""), "")
  )),
  ("capitalize — single char", () => (
    t.assertions.assert_eq(m.capitalize("a"), "A")
  )),
  ("capitalize — only first char changed", () => (
    t.assertions.assert_eq(m.capitalize("hELLO"), "HELLO")
  )),
  ("uncapitalize — uppercase first letter", () => (
    t.assertions.assert_eq(m.uncapitalize("Hello"), "hello")
  )),
  ("uncapitalize — already lowercase", () => (
    t.assertions.assert_eq(m.uncapitalize("hello"), "hello")
  )),
  ("uncapitalize — empty string", () => (
    t.assertions.assert_eq(m.uncapitalize(""), "")
  )),
  ("uncapitalize — single char", () => (
    t.assertions.assert_eq(m.uncapitalize("A"), "a")
  )),
  ("uncapitalize — only first char changed", () => (
    t.assertions.assert_eq(m.uncapitalize("HELLO"), "hELLO")
  )),
]));
