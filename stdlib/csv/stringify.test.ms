//! Tests for CSV serializer.

import "@std/testing" as t;
import "@std/csv/stringify" as csv;

t.describe("CSV.stringify", () => (
  t.it("stringify - simple rows", () => (
    let result := csv.stringify([["a", "b", "c"], ["1", "2", "3"]]);
    t.expect(result).toBe("a,b,c\n1,2,3")
  ));
  t.it("stringify - single row", () => (
    let result := csv.stringify([["x", "y"]]);
    t.expect(result).toBe("x,y")
  ));
  t.it("stringify - empty rows array", () => (
    let result := csv.stringify([]);
    t.expect(result).toBe("")
  ));
  t.it("stringify - field with comma gets quoted", () => (
    let result := csv.stringify([["a,b", "c"]]);
    t.expect(result).toBe("\"a,b\",c")
  ));
  t.it("stringify - field with quote gets escaped and quoted", () => (
    let result := csv.stringify([["say \"hi\"", "c"]]);
    t.expect(result).toBe("\"say \"\"hi\"\"\",c")
  ));
  t.it("stringify - field with newline gets quoted", () => (
    let result := csv.stringify([["line1\nline2", "c"]]);
    t.expect(result).toBe("\"line1\nline2\",c")
  ));
  t.it("stringify - plain fields need no quoting", () => (
    let result := csv.stringify([["hello", "world"]]);
    t.expect(result).toBe("hello,world")
  ));
  t.it("stringify - multiple rows joined by newline", () => (
    let result := csv.stringify([["r1c1", "r1c2"], ["r2c1", "r2c2"]]);
    t.expect(result).toBe("r1c1,r1c2\nr2c1,r2c2")
  ));
  t.it("stringify - empty field in row", () => (
    let result := csv.stringify([["a", "", "c"]]);
    t.expect(result).toBe("a,,c")
  ))
));

t.run();
