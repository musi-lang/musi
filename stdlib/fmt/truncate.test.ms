//! Tests for string truncation with suffix.

import "@std/testing" as t;
import "@std/fmt/truncate" as m;

t.runner.report(t.runner.run_suite([
  ("truncate - short string unchanged", () => (
    t.assertions.assert_eq(m.truncate("hello", 10, "..."), "hello")
  )),
  ("truncate - exact length unchanged", () => (
    t.assertions.assert_eq(m.truncate("hello", 5, "..."), "hello")
  )),
  ("truncate - long string gets suffix", () => (
    t.assertions.assert_eq(m.truncate("hello world", 8, "..."), "hello...")
  )),
  ("truncate - empty string unchanged", () => (
    t.assertions.assert_eq(m.truncate("", 5, "..."), "")
  )),
  ("truncate - empty suffix", () => (
    t.assertions.assert_eq(m.truncate("hello world", 5, ""), "hello")
  )),
  ("truncate - suffix fills max_len", () => (
    t.assertions.assert_eq(m.truncate("abcdef", 3, "..."), "...")
  )),
  ("truncate - single char suffix", () => (
    t.assertions.assert_eq(m.truncate("hello world", 6, "~"), "hello~")
  )),
]));
