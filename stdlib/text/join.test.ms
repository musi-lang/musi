//! Tests for string join utilities.

import "@std/testing" as t;
import "@std/text/join" as tj;

t.describe("Text.join", () => (
  t.it("join - joins with separator", () => (
    let result := tj.join(["a", "b", "c"], ", ");
    t.expect(result).toBe("a, b, c")
  ));
  t.it("join - single element no separator", () => (
    let result := tj.join(["hello"], ", ");
    t.expect(result).toBe("hello")
  ));
  t.it("join - empty array returns empty string", () => (
    let result := tj.join([], ", ");
    t.expect(result).toBe("")
  ));
  t.it("join - empty separator concatenates", () => (
    let result := tj.join(["a", "b", "c"], "");
    t.expect(result).toBe("abc")
  ));
  t.it("join - newline separator", () => (
    let result := tj.join(["line1", "line2", "line3"], "\n");
    t.expect(result).toBe("line1\nline2\nline3")
  ));
  t.it("join - multi-char separator", () => (
    let result := tj.join(["x", "y"], " | ");
    t.expect(result).toBe("x | y")
  ));
  t.it("join - two elements", () => (
    let result := tj.join(["foo", "bar"], "-");
    t.expect(result).toBe("foo-bar")
  ))
));

t.run();
