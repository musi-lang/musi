//! Tests for JSON serializer.

import "@std/testing" as t;
import "@std/json/stringify" as json;

t.describe("JSON.stringify", () => (
  t.it("stringify - null", () => (
    let result := json.stringify(.Null);
    t.expect(result).toBe("null")
  ));
  t.it("stringify - true", () => (
    let result := json.stringify(.Bool(.True));
    t.expect(result).toBe("true")
  ));
  t.it("stringify - false", () => (
    let result := json.stringify(.Bool(.False));
    t.expect(result).toBe("false")
  ));
  t.it("stringify - integer number", () => (
    let result := json.stringify(.Number(42.0));
    t.expect(result).toBe("42")
  ));
  t.it("stringify - simple string", () => (
    let result := json.stringify(.Str("hello"));
    t.expect(result).toBe("\"hello\"")
  ));
  t.it("stringify - empty string", () => (
    let result := json.stringify(.Str(""));
    t.expect(result).toBe("\"\"")
  ));
  t.it("stringify - string with quote escaping", () => (
    let result := json.stringify(.Str("say \"hi\""));
    t.expect(result).toBe("\"say \\\"hi\\\"\"")
  ));
  t.it("stringify - string with newline escaping", () => (
    let result := json.stringify(.Str("line1\nline2"));
    t.expect(result).toBe("\"line1\\nline2\"")
  ));
  t.it("stringify - string with backslash escaping", () => (
    let result := json.stringify(.Str("a\\b"));
    t.expect(result).toBe("\"a\\\\b\"")
  ));
  t.it("stringify - empty array", () => (
    let result := json.stringify(.Array([]));
    t.expect(result).toBe("[]")
  ));
  t.it("stringify - array of numbers", () => (
    let result := json.stringify(.Array([.Number(1.0), .Number(2.0), .Number(3.0)]));
    t.expect(result).toBe("[1, 2, 3]")
  ));
  t.it("stringify - empty object", () => (
    let result := json.stringify(.Object([]));
    t.expect(result).toBe("{}")
  ));
  t.it("stringify - object with one key", () => (
    let result := json.stringify(.Object([("x", .Number(1.0))]));
    t.expect(result).toBe("{\"x\": 1}")
  ));
  t.it("stringify - nested structure", () => (
    let result := json.stringify(.Object([("a", .Array([.Bool(.True), .Null]))]));
    t.expect(result).toBe("{\"a\": [true, null]}")
  ))
));

t.run();
