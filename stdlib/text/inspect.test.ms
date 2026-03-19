//! Tests for string inspection predicates.

import "@std/testing" as t;
import "@std/text/inspect" as m;

t.describe("Text.inspect", () => (
  t.it("is_blank - empty string", () => (
    t.expect(m.is_blank("")).toBe(.True)
  ));
  t.it("is_blank - whitespace only", () => (
    t.expect(m.is_blank("   ")).toBe(.True)
  ));
  t.it("is_blank - non-whitespace", () => (
    t.expect(m.is_blank("hello")).toBe(.False)
  ));
  t.it("is_blank - mixed whitespace and text", () => (
    t.expect(m.is_blank("  hi  ")).toBe(.False)
  ));
  t.it("is_alpha - letters only", () => (
    t.expect(m.is_alpha("abc")).toBe(.True)
  ));
  t.it("is_alpha - empty string", () => (
    t.expect(m.is_alpha("")).toBe(.False)
  ));
  t.it("is_alpha - contains digit", () => (
    t.expect(m.is_alpha("abc1")).toBe(.False)
  ));
  t.it("is_alpha - single letter", () => (
    t.expect(m.is_alpha("z")).toBe(.True)
  ));
  t.it("is_digit - digits only", () => (
    t.expect(m.is_digit("123")).toBe(.True)
  ));
  t.it("is_digit - empty string", () => (
    t.expect(m.is_digit("")).toBe(.False)
  ));
  t.it("is_digit - contains letter", () => (
    t.expect(m.is_digit("12a")).toBe(.False)
  ));
  t.it("is_alphanumeric - letters and digits", () => (
    t.expect(m.is_alphanumeric("abc123")).toBe(.True)
  ));
  t.it("is_alphanumeric - empty string", () => (
    t.expect(m.is_alphanumeric("")).toBe(.False)
  ));
  t.it("is_alphanumeric - contains space", () => (
    t.expect(m.is_alphanumeric("abc 123")).toBe(.False)
  ));
  t.it("is_alphanumeric - letters only", () => (
    t.expect(m.is_alphanumeric("hello")).toBe(.True)
  ))
));

t.run();
