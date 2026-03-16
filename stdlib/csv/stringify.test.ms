//! Tests for CSV serializer.

import "@std/testing" as t;
import "@std/csv/stringify" as csv;

t.runner.report(t.runner.run_suite([
  ("stringify — simple rows", () => (
    let result := csv.stringify([["a", "b", "c"], ["1", "2", "3"]]);
    t.assertions.assert_eq(result, "a,b,c\n1,2,3")
  )),
  ("stringify — single row", () => (
    let result := csv.stringify([["x", "y"]]);
    t.assertions.assert_eq(result, "x,y")
  )),
  ("stringify — empty rows array", () => (
    let result := csv.stringify([]);
    t.assertions.assert_eq(result, "")
  )),
  ("stringify — field with comma gets quoted", () => (
    let result := csv.stringify([["a,b", "c"]]);
    t.assertions.assert_eq(result, "\"a,b\",c")
  )),
  ("stringify — field with quote gets escaped and quoted", () => (
    let result := csv.stringify([["say \"hi\"", "c"]]);
    t.assertions.assert_eq(result, "\"say \"\"hi\"\"\",c")
  )),
  ("stringify — field with newline gets quoted", () => (
    let result := csv.stringify([["line1\nline2", "c"]]);
    t.assertions.assert_eq(result, "\"line1\nline2\",c")
  )),
  ("stringify — plain fields need no quoting", () => (
    let result := csv.stringify([["hello", "world"]]);
    t.assertions.assert_eq(result, "hello,world")
  )),
  ("stringify — multiple rows joined by newline", () => (
    let result := csv.stringify([["r1c1", "r1c2"], ["r2c1", "r2c2"]]);
    t.assertions.assert_eq(result, "r1c1,r1c2\nr2c1,r2c2")
  )),
  ("stringify — empty field in row", () => (
    let result := csv.stringify([["a", "", "c"]]);
    t.assertions.assert_eq(result, "a,,c")
  )),
]));
