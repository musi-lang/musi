//! Tests for string transformation utilities.

import "@std/testing" as t;
import "@std/text/transform" as m;

t.describe("Text.transform", () => (
  t.it("reverse_str - reverses characters", () => (
    t.expect(m.reverse_str("hello")).toBe("olleh")
  ));
  t.it("reverse_str - empty string", () => (
    t.expect(m.reverse_str("")).toBe("")
  ));
  t.it("reverse_str - single character", () => (
    t.expect(m.reverse_str("a")).toBe("a")
  ));
  t.it("reverse_str - palindrome unchanged", () => (
    t.expect(m.reverse_str("racecar")).toBe("racecar")
  ));
  t.it("truncate - short string unchanged", () => (
    t.expect(m.truncate("hi", 10)).toBe("hi")
  ));
  t.it("truncate - exact length unchanged", () => (
    t.expect(m.truncate("hello", 5)).toBe("hello")
  ));
  t.it("truncate - long string cut", () => (
    t.expect(m.truncate("hello world", 5)).toBe("hello")
  ));
  t.it("truncate - empty string", () => (
    t.expect(m.truncate("", 5)).toBe("")
  ));
  t.it("remove_prefix - removes matching prefix", () => (
    t.expect(m.remove_prefix("foobar", "foo")).toBe("bar")
  ));
  t.it("remove_prefix - no match unchanged", () => (
    t.expect(m.remove_prefix("foobar", "baz")).toBe("foobar")
  ));
  t.it("remove_prefix - empty prefix", () => (
    t.expect(m.remove_prefix("hello", "")).toBe("hello")
  ));
  t.it("remove_suffix - removes matching suffix", () => (
    t.expect(m.remove_suffix("foobar", "bar")).toBe("foo")
  ));
  t.it("remove_suffix - no match unchanged", () => (
    t.expect(m.remove_suffix("foobar", "baz")).toBe("foobar")
  ));
  t.it("remove_suffix - empty suffix", () => (
    t.expect(m.remove_suffix("hello", "")).toBe("hello")
  ))
));

t.run();
