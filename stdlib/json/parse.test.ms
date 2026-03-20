//! Tests for JSON parser.

import "@std/testing" as t;
import "@std/json/parse" as json;
import "@std/result/result" as res;

t.describe("JSON.parse", () => (
  t.it("parse - null", () => (
    let result := json.parse("null");
    t.expect(result).toBe(.Ok(.Null))
  ));
  t.it("parse - true", () => (
    let result := json.parse("true");
    t.expect(result).toBe(.Ok(.Bool(.True)))
  ));
  t.it("parse - false", () => (
    let result := json.parse("false");
    t.expect(result).toBe(.Ok(.Bool(.False)))
  ));
  t.it("parse - positive integer", () => (
    let result := json.parse("42");
    t.expect(result).toBe(.Ok(.Number(42.0)))
  ));
  t.it("parse - negative integer", () => (
    let result := json.parse("-7");
    t.expect(result).toBe(.Ok(.Number(-7.0)))
  ));
  t.it("parse - floating point number", () => (
    let result := json.parse("3.14");
    t.expect(result).toBe(.Ok(.Number(3.14)))
  ));
  t.it("parse - simple string", () => (
    let result := json.parse("\"hello\"");
    t.expect(result).toBe(.Ok(.Str("hello")))
  ));
  t.it("parse - empty string", () => (
    let result := json.parse("\"\"");
    t.expect(result).toBe(.Ok(.Str("")))
  ));
  t.it("parse - string with escaped quote", () => (
    let result := json.parse("\"say \\\"hi\\\"\"");
    t.expect(result).toBe(.Ok(.Str("say \"hi\"")))
  ));
  t.it("parse - string with escaped newline", () => (
    let result := json.parse("\"line1\\nline2\"");
    t.expect(result).toBe(.Ok(.Str("line1\nline2")))
  ));
  t.it("parse - string with escaped tab", () => (
    let result := json.parse("\"col1\\tcol2\"");
    t.expect(result).toBe(.Ok(.Str("col1\tcol2")))
  ));
  t.it("parse - empty array", () => (
    let result := json.parse("[]");
    t.expect(result).toBe(.Ok(.Array([])))
  ));
  t.it("parse - array of numbers", () => (
    let result := json.parse("[1, 2, 3]");
    t.expect(result).toBe(.Ok(.Array([.Number(1.0), .Number(2.0), .Number(3.0)])))
  ));
  t.it("parse - empty object", () => (
    let result := json.parse("{}");
    t.expect(result).toBe(.Ok(.Object([])))
  ));
  t.it("parse - object with one key", () => (
    let result := json.parse("{\"x\": 1}");
    t.expect(result).toBe(.Ok(.Object([("x", .Number(1.0))])))
  ));
  t.it("parse - nested object", () => (
    let result := json.parse("{\"a\": {\"b\": true}}");
    t.expect(result).toBe(.Ok(.Object([("a", .Object([("b", .Bool(.True))]))])))
  ));
  t.it("parse - array of mixed types", () => (
    let result := json.parse("[null, true, 1]");
    t.expect(result).toBe(.Ok(.Array([.Null, .Bool(.True), .Number(1.0)])))
  ));
  t.it("parse - whitespace around value", () => (
    let result := json.parse("  42  ");
    t.expect(result).toBe(.Ok(.Number(42.0)))
  ));
  t.it("parse - empty input returns error", () => (
    let result := json.parse("");
    t.expect(res.is_err(result)).toBe(.True)
  ));
  t.it("parse - invalid token returns error", () => (
    let result := json.parse("xyz");
    t.expect(res.is_err(result)).toBe(.True)
  ))
));

t.run();
