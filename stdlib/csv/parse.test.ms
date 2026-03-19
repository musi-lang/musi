//! Tests for CSV parser.

import "@std/testing" as t;
import "@std/csv/parse" as csv;

t.describe("CSV.parse", () => (
  t.it("parse_row - simple fields", () => (
    let result := csv.parse_row("a,b,c");
    t.expect(result).toBe(["a", "b", "c"])
  ));
  t.it("parse_row - single field", () => (
    let result := csv.parse_row("hello");
    t.expect(result).toBe(["hello"])
  ));
  t.it("parse_row - empty field between commas", () => (
    let result := csv.parse_row("a,,c");
    t.expect(result).toBe(["a", "", "c"])
  ));
  t.it("parse_row - quoted field", () => (
    let result := csv.parse_row("\"hello world\",b");
    t.expect(result).toBe(["hello world", "b"])
  ));
  t.it("parse_row - quoted field with comma inside", () => (
    let result := csv.parse_row("\"a,b\",c");
    t.expect(result).toBe(["a,b", "c"])
  ));
  t.it("parse_row - escaped quote inside quoted field", () => (
    let result := csv.parse_row("\"say \"\"hi\"\"\",b");
    t.expect(result).toBe(["say \"hi\"", "b"])
  ));
  t.it("parse_row - numeric fields", () => (
    let result := csv.parse_row("1,2,3");
    t.expect(result).toBe(["1", "2", "3"])
  ));
  t.it("parse - multiple rows", () => (
    let result := csv.parse("a,b\n1,2\n3,4");
    t.expect(result).toBe([["a", "b"], ["1", "2"], ["3", "4"]])
  ));
  t.it("parse - skips blank lines", () => (
    let result := csv.parse("a,b\n\n1,2");
    t.expect(result).toBe([["a", "b"], ["1", "2"]])
  ));
  t.it("parse - single row", () => (
    let result := csv.parse("x,y,z");
    t.expect(result).toBe([["x", "y", "z"]])
  ));
  t.it("parse - empty input returns empty", () => (
    let result := csv.parse("");
    t.expect(result).toBe([])
  ));
  t.it("parse - trailing newline ignored", () => (
    let result := csv.parse("a,b\n");
    t.expect(result).toBe([["a", "b"]])
  ))
));

t.run();
