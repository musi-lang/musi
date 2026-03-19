//! Tests for string case transformations.

import "@std/testing" as t;
import "@std/text/case" as m;

t.describe("Text.case", () => (
  t.it("capitalize - lowercase first letter", () => (
    t.expect(m.capitalize("hello")).toBe("Hello")
  ));
  t.it("capitalize - already capitalized", () => (
    t.expect(m.capitalize("Hello")).toBe("Hello")
  ));
  t.it("capitalize - empty string", () => (
    t.expect(m.capitalize("")).toBe("")
  ));
  t.it("capitalize - single char", () => (
    t.expect(m.capitalize("a")).toBe("A")
  ));
  t.it("capitalize - only first char changed", () => (
    t.expect(m.capitalize("hELLO")).toBe("HELLO")
  ));
  t.it("uncapitalize - uppercase first letter", () => (
    t.expect(m.uncapitalize("Hello")).toBe("hello")
  ));
  t.it("uncapitalize - already lowercase", () => (
    t.expect(m.uncapitalize("hello")).toBe("hello")
  ));
  t.it("uncapitalize - empty string", () => (
    t.expect(m.uncapitalize("")).toBe("")
  ));
  t.it("uncapitalize - single char", () => (
    t.expect(m.uncapitalize("A")).toBe("a")
  ));
  t.it("uncapitalize - only first char changed", () => (
    t.expect(m.uncapitalize("HELLO")).toBe("hELLO")
  ))
));

t.run();
