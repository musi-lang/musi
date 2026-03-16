//! Tests for CSV parser.

import "@std/testing" as t;
import "@std/csv/parse" as csv;

t.runner.report(t.runner.run_suite([
  ("parse_row — simple fields", () => (
    let result := csv.parse_row("a,b,c");
    t.assertions.assert_eq(result, ["a", "b", "c"])
  )),
  ("parse_row — single field", () => (
    let result := csv.parse_row("hello");
    t.assertions.assert_eq(result, ["hello"])
  )),
  ("parse_row — empty field between commas", () => (
    let result := csv.parse_row("a,,c");
    t.assertions.assert_eq(result, ["a", "", "c"])
  )),
  ("parse_row — quoted field", () => (
    let result := csv.parse_row("\"hello world\",b");
    t.assertions.assert_eq(result, ["hello world", "b"])
  )),
  ("parse_row — quoted field with comma inside", () => (
    let result := csv.parse_row("\"a,b\",c");
    t.assertions.assert_eq(result, ["a,b", "c"])
  )),
  ("parse_row — escaped quote inside quoted field", () => (
    let result := csv.parse_row("\"say \"\"hi\"\"\",b");
    t.assertions.assert_eq(result, ["say \"hi\"", "b"])
  )),
  ("parse_row — numeric fields", () => (
    let result := csv.parse_row("1,2,3");
    t.assertions.assert_eq(result, ["1", "2", "3"])
  )),
  ("parse — multiple rows", () => (
    let result := csv.parse("a,b\n1,2\n3,4");
    t.assertions.assert_eq(result, [["a", "b"], ["1", "2"], ["3", "4"]])
  )),
  ("parse — skips blank lines", () => (
    let result := csv.parse("a,b\n\n1,2");
    t.assertions.assert_eq(result, [["a", "b"], ["1", "2"]])
  )),
  ("parse — single row", () => (
    let result := csv.parse("x,y,z");
    t.assertions.assert_eq(result, [["x", "y", "z"]])
  )),
  ("parse — empty input returns empty", () => (
    let result := csv.parse("");
    t.assertions.assert_eq(result, [])
  )),
  ("parse — trailing newline ignored", () => (
    let result := csv.parse("a,b\n");
    t.assertions.assert_eq(result, [["a", "b"]])
  )),
]));
