//! Tests for string splitting utilities.

import "@std/testing" as t;
import "@std/text/split" as ts;

t.describe("Text.split", () => (
  t.it("lines - splits on newline", () => (
    let result := ts.lines("a\nb\nc");
    t.expect(result).toBe(["a", "b", "c"])
  ));
  t.it("lines - single line with no newline", () => (
    let result := ts.lines("hello");
    t.expect(result).toBe(["hello"])
  ));
  t.it("lines - empty string gives one empty element", () => (
    let result := ts.lines("");
    t.expect(result).toBe([""])
  ));
  t.it("lines - trailing newline produces empty last element", () => (
    let result := ts.lines("a\nb\n");
    t.expect(result).toBe(["a", "b", ""])
  ));
  t.it("words - splits on whitespace", () => (
    let result := ts.words("hello world foo");
    t.expect(result).toBe(["hello", "world", "foo"])
  ));
  t.it("words - leading and trailing spaces trimmed", () => (
    let result := ts.words("  hello  world  ");
    t.expect(result).toBe(["hello", "world"])
  ));
  t.it("words - multiple spaces between words collapsed", () => (
    let result := ts.words("a  b  c");
    t.expect(result).toBe(["a", "b", "c"])
  ));
  t.it("words - single word", () => (
    let result := ts.words("hello");
    t.expect(result).toBe(["hello"])
  ));
  t.it("words - empty string returns empty array", () => (
    let result := ts.words("");
    t.expect(result).toBe([])
  ));
  t.it("words - whitespace-only string returns empty array", () => (
    let result := ts.words("   ");
    t.expect(result).toBe([])
  ))
));

t.run();
