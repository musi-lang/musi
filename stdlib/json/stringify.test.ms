//! Tests for JSON serializer.

import "@std/testing" as t;
import "@std/json/stringify" as json;

t.runner.report(t.runner.run_suite([
  ("stringify - null", () => (
    let result := json.stringify(.Null);
    t.assertions.assert_eq(result, "null")
  )),
  ("stringify - true", () => (
    let result := json.stringify(.Bool(.True));
    t.assertions.assert_eq(result, "true")
  )),
  ("stringify - false", () => (
    let result := json.stringify(.Bool(.False));
    t.assertions.assert_eq(result, "false")
  )),
  ("stringify - integer number", () => (
    let result := json.stringify(.Number(42.0));
    t.assertions.assert_eq(result, "42")
  )),
  ("stringify - simple string", () => (
    let result := json.stringify(.Str("hello"));
    t.assertions.assert_eq(result, "\"hello\"")
  )),
  ("stringify - empty string", () => (
    let result := json.stringify(.Str(""));
    t.assertions.assert_eq(result, "\"\"")
  )),
  ("stringify - string with quote escaping", () => (
    let result := json.stringify(.Str("say \"hi\""));
    t.assertions.assert_eq(result, "\"say \\\"hi\\\"\"")
  )),
  ("stringify - string with newline escaping", () => (
    let result := json.stringify(.Str("line1\nline2"));
    t.assertions.assert_eq(result, "\"line1\\nline2\"")
  )),
  ("stringify - string with backslash escaping", () => (
    let result := json.stringify(.Str("a\\b"));
    t.assertions.assert_eq(result, "\"a\\\\b\"")
  )),
  ("stringify - empty array", () => (
    let result := json.stringify(.Array([]));
    t.assertions.assert_eq(result, "[]")
  )),
  ("stringify - array of numbers", () => (
    let result := json.stringify(.Array([.Number(1.0), .Number(2.0), .Number(3.0)]));
    t.assertions.assert_eq(result, "[1, 2, 3]")
  )),
  ("stringify - empty object", () => (
    let result := json.stringify(.Object([]));
    t.assertions.assert_eq(result, "{}")
  )),
  ("stringify - object with one key", () => (
    let result := json.stringify(.Object([("x", .Number(1.0))]));
    t.assertions.assert_eq(result, "{\"x\": 1}")
  )),
  ("stringify - nested structure", () => (
    let result := json.stringify(.Object([("a", .Array([.Bool(.True), .Null]))]));
    t.assertions.assert_eq(result, "{\"a\": [true, null]}")
  )),
]));
