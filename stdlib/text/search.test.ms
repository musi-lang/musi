//! Tests for string search utilities.

import "@std/testing" as t;
import "@std/text/search" as ts;

t.describe("Text.search", () => (
  t.it("contains - substring present returns true", () => (
    let result := ts.contains("hello world", "world");
    t.expect(result).toBe(.True)
  ));
  t.it("contains - substring absent returns false", () => (
    let result := ts.contains("hello world", "xyz");
    t.expect(result).toBe(.False)
  ));
  t.it("contains - empty substring returns true", () => (
    let result := ts.contains("hello", "");
    t.expect(result).toBe(.True)
  ));
  t.it("contains - match at start", () => (
    let result := ts.contains("hello", "hel");
    t.expect(result).toBe(.True)
  ));
  t.it("contains - match at end", () => (
    let result := ts.contains("hello", "llo");
    t.expect(result).toBe(.True)
  ));
  t.it("count_occurrences - counts non-overlapping matches", () => (
    let result := ts.count_occurrences("abababab", "ab");
    t.expect(result).toBe(4)
  ));
  t.it("count_occurrences - no matches returns zero", () => (
    let result := ts.count_occurrences("hello", "xyz");
    t.expect(result).toBe(0)
  ));
  t.it("count_occurrences - empty substring returns zero", () => (
    let result := ts.count_occurrences("hello", "");
    t.expect(result).toBe(0)
  ));
  t.it("count_occurrences - single match", () => (
    let result := ts.count_occurrences("hello world", "world");
    t.expect(result).toBe(1)
  ));
  t.it("last_index_of - finds last occurrence", () => (
    let result := ts.last_index_of("abcabc", "bc");
    t.expect(result).toBe(4)
  ));
  t.it("last_index_of - single occurrence", () => (
    let result := ts.last_index_of("hello world", "world");
    t.expect(result).toBe(6)
  ));
  t.it("last_index_of - not found returns -1", () => (
    let result := ts.last_index_of("hello", "xyz");
    t.expect(result).toBe(-1)
  ));
  t.it("last_index_of - empty substring returns -1", () => (
    let result := ts.last_index_of("hello", "");
    t.expect(result).toBe(-1)
  ))
));

t.run();
