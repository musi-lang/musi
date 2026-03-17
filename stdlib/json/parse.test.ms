//! Tests for JSON parser.

import "@std/testing" as t;
import "@std/json/parse" as json;
import "@std/result/result" as res;

t.runner.report(t.runner.run_suite([
  ("parse - null", () => (
    let result := json.parse("null");
    t.assertions.assert_eq(result, .Ok(.Null))
  )),
  ("parse - true", () => (
    let result := json.parse("true");
    t.assertions.assert_eq(result, .Ok(.Bool(.True)))
  )),
  ("parse - false", () => (
    let result := json.parse("false");
    t.assertions.assert_eq(result, .Ok(.Bool(.False)))
  )),
  ("parse - positive integer", () => (
    let result := json.parse("42");
    t.assertions.assert_eq(result, .Ok(.Number(42.0)))
  )),
  ("parse - negative integer", () => (
    let result := json.parse("-7");
    t.assertions.assert_eq(result, .Ok(.Number(-7.0)))
  )),
  ("parse - floating point number", () => (
    let result := json.parse("3.14");
    t.assertions.assert_eq(result, .Ok(.Number(3.14)))
  )),
  ("parse - simple string", () => (
    let result := json.parse("\"hello\"");
    t.assertions.assert_eq(result, .Ok(.Str("hello")))
  )),
  ("parse - empty string", () => (
    let result := json.parse("\"\"");
    t.assertions.assert_eq(result, .Ok(.Str("")))
  )),
  ("parse - string with escaped quote", () => (
    let result := json.parse("\"say \\\"hi\\\"\"");
    t.assertions.assert_eq(result, .Ok(.Str("say \"hi\"")))
  )),
  ("parse - string with escaped newline", () => (
    let result := json.parse("\"line1\\nline2\"");
    t.assertions.assert_eq(result, .Ok(.Str("line1\nline2")))
  )),
  ("parse - string with escaped tab", () => (
    let result := json.parse("\"col1\\tcol2\"");
    t.assertions.assert_eq(result, .Ok(.Str("col1\tcol2")))
  )),
  ("parse - empty array", () => (
    let result := json.parse("[]");
    t.assertions.assert_eq(result, .Ok(.Array([])))
  )),
  ("parse - array of numbers", () => (
    let result := json.parse("[1, 2, 3]");
    t.assertions.assert_eq(result, .Ok(.Array([.Number(1.0), .Number(2.0), .Number(3.0)])))
  )),
  ("parse - empty object", () => (
    let result := json.parse("{}");
    t.assertions.assert_eq(result, .Ok(.Object([])))
  )),
  ("parse - object with one key", () => (
    let result := json.parse("{\"x\": 1}");
    t.assertions.assert_eq(result, .Ok(.Object([("x", .Number(1.0))])))
  )),
  ("parse - nested object", () => (
    let result := json.parse("{\"a\": {\"b\": true}}");
    t.assertions.assert_eq(result, .Ok(.Object([("a", .Object([("b", .Bool(.True))]))])))
  )),
  ("parse - array of mixed types", () => (
    let result := json.parse("[null, true, 1]");
    t.assertions.assert_eq(result, .Ok(.Array([.Null, .Bool(.True), .Number(1.0)])))
  )),
  ("parse - whitespace around value", () => (
    let result := json.parse("  42  ");
    t.assertions.assert_eq(result, .Ok(.Number(42.0)))
  )),
  ("parse - empty input returns error", () => (
    let result := json.parse("");
    t.assertions.assert_true(res.is_err(result))
  )),
  ("parse - invalid token returns error", () => (
    let result := json.parse("xyz");
    t.assertions.assert_true(res.is_err(result))
  )),
]));
