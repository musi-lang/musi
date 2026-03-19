//! Tests for string builder.

import "@std/testing" as t;
import "@std/text/builder" as m;

t.describe("Text.builder", () => (
  t.it("new - creates builder with zero parts", () => (
    let b := m.new();
    t.expect(m.len(b)).toBe(0)
  ));
  t.it("append - increases part count", () => (
    let b := m.new();
    m.append(b, "hello");
    t.expect(m.len(b)).toBe(1)
  ));
  t.it("build - empty builder gives empty string", () => (
    let b := m.new();
    t.expect(m.build(b)).toBe("")
  ));
  t.it("build - single append", () => (
    let b := m.new();
    m.append(b, "hello");
    t.expect(m.build(b)).toBe("hello")
  ));
  t.it("build - multiple appends concatenated", () => (
    let b := m.new();
    m.append(b, "foo");
    m.append(b, "bar");
    t.expect(m.build(b)).toBe("foobar")
  ));
  t.it("append_line - adds newline", () => (
    let b := m.new();
    m.append_line(b, "hello");
    t.expect(m.build(b)).toBe("hello\n")
  ));
  t.it("append_line - multiple lines", () => (
    let b := m.new();
    m.append_line(b, "a");
    m.append_line(b, "b");
    t.expect(m.build(b)).toBe("a\nb\n")
  ));
  t.it("len - reflects append count", () => (
    let b := m.new();
    m.append(b, "x");
    m.append(b, "y");
    m.append(b, "z");
    t.expect(m.len(b)).toBe(3)
  ))
));

t.run();
