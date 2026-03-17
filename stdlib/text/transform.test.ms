//! Tests for string transformation utilities.

import "@std/testing" as t;
import "@std/text/transform" as m;

t.runner.report(t.runner.run_suite([
  ("reverse_str - reverses characters", () => (
    t.assertions.assert_eq(m.reverse_str("hello"), "olleh")
  )),
  ("reverse_str - empty string", () => (
    t.assertions.assert_eq(m.reverse_str(""), "")
  )),
  ("reverse_str - single character", () => (
    t.assertions.assert_eq(m.reverse_str("a"), "a")
  )),
  ("reverse_str - palindrome unchanged", () => (
    t.assertions.assert_eq(m.reverse_str("racecar"), "racecar")
  )),
  ("truncate - short string unchanged", () => (
    t.assertions.assert_eq(m.truncate("hi", 10), "hi")
  )),
  ("truncate - exact length unchanged", () => (
    t.assertions.assert_eq(m.truncate("hello", 5), "hello")
  )),
  ("truncate - long string cut", () => (
    t.assertions.assert_eq(m.truncate("hello world", 5), "hello")
  )),
  ("truncate - empty string", () => (
    t.assertions.assert_eq(m.truncate("", 5), "")
  )),
  ("remove_prefix - removes matching prefix", () => (
    t.assertions.assert_eq(m.remove_prefix("foobar", "foo"), "bar")
  )),
  ("remove_prefix - no match unchanged", () => (
    t.assertions.assert_eq(m.remove_prefix("foobar", "baz"), "foobar")
  )),
  ("remove_prefix - empty prefix", () => (
    t.assertions.assert_eq(m.remove_prefix("hello", ""), "hello")
  )),
  ("remove_suffix - removes matching suffix", () => (
    t.assertions.assert_eq(m.remove_suffix("foobar", "bar"), "foo")
  )),
  ("remove_suffix - no match unchanged", () => (
    t.assertions.assert_eq(m.remove_suffix("foobar", "baz"), "foobar")
  )),
  ("remove_suffix - empty suffix", () => (
    t.assertions.assert_eq(m.remove_suffix("hello", ""), "hello")
  )),
]));
