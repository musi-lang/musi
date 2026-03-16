//! Tests for string builder.

import "@std/testing" as t;
import "@std/text/builder" as m;

t.runner.report(t.runner.run_suite([
  ("new — creates builder with zero parts", () => (
    let b := m.new();
    t.assertions.assert_eq(m.len(b), 0)
  )),
  ("append — increases part count", () => (
    let b := m.new();
    m.append(b, "hello");
    t.assertions.assert_eq(m.len(b), 1)
  )),
  ("build — empty builder gives empty string", () => (
    let b := m.new();
    t.assertions.assert_eq(m.build(b), "")
  )),
  ("build — single append", () => (
    let b := m.new();
    m.append(b, "hello");
    t.assertions.assert_eq(m.build(b), "hello")
  )),
  ("build — multiple appends concatenated", () => (
    let b := m.new();
    m.append(b, "foo");
    m.append(b, "bar");
    t.assertions.assert_eq(m.build(b), "foobar")
  )),
  ("append_line — adds newline", () => (
    let b := m.new();
    m.append_line(b, "hello");
    t.assertions.assert_eq(m.build(b), "hello\n")
  )),
  ("append_line — multiple lines", () => (
    let b := m.new();
    m.append_line(b, "a");
    m.append_line(b, "b");
    t.assertions.assert_eq(m.build(b), "a\nb\n")
  )),
  ("len — reflects append count", () => (
    let b := m.new();
    m.append(b, "x");
    m.append(b, "y");
    m.append(b, "z");
    t.assertions.assert_eq(m.len(b), 3)
  )),
]));
