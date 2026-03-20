//! Tests for result type and combinators.

import "@std/testing" as t;
import "@std/result/result" as res;

t.describe("Result", () => (
  t.it("is_ok - Ok returns true", () => (
    t.expect(res.is_ok(.Ok(1))).toBe(.True)
  ));
  t.it("is_ok - Err returns false", () => (
    t.expect(res.is_ok(.Err("oops"))).toBe(.False)
  ));
  t.it("is_err - Err returns true", () => (
    t.expect(res.is_err(.Err("bad"))).toBe(.True)
  ));
  t.it("is_err - Ok returns false", () => (
    t.expect(res.is_err(.Ok(42))).toBe(.False)
  ));
  t.it("map_ok - Ok applies function", () => (
    let result := res.map_ok(.Ok(5), (x) => x * 2);
    t.expect(result).toBe(.Ok(10))
  ));
  t.it("map_ok - Err passes through unchanged", () => (
    let result := res.map_ok(.Err("e"), (x) => x * 2);
    t.expect(result).toBe(.Err("e"))
  ));
  t.it("map_err - Err applies function", () => (
    let result := res.map_err(.Err("bad"), (e) => e + "!");
    t.expect(result).toBe(.Err("bad!"))
  ));
  t.it("map_err - Ok passes through unchanged", () => (
    let result := res.map_err(.Ok(3), (e) => e + "!");
    t.expect(result).toBe(.Ok(3))
  ));
  t.it("unwrap_or - Ok returns inner value", () => (
    let result := res.unwrap_or(.Ok(7), 0);
    t.expect(result).toBe(7)
  ));
  t.it("unwrap_or - Err returns default", () => (
    let result := res.unwrap_or(.Err("fail"), 0);
    t.expect(result).toBe(0)
  ));
  t.it("unwrap_or_else - Ok returns inner value", () => (
    let result := res.unwrap_or_else(.Ok(9), (_ : String) => 0);
    t.expect(result).toBe(9)
  ));
  t.it("unwrap_or_else - Err calls function with error", () => (
    let result := res.unwrap_or_else(.Err("x"), (_ : String) => 99);
    t.expect(result).toBe(99)
  ));
  t.it("and_then - Ok applies function returning Ok", () => (
    let result := res.and_then(.Ok(4), (x) => .Ok(x + 1));
    t.expect(result).toBe(.Ok(5))
  ));
  t.it("and_then - Ok applies function returning Err", () => (
    let result := res.and_then(.Ok(4), (_ : Int) => .Err("nope"));
    t.expect(result).toBe(.Err("nope"))
  ));
  t.it("and_then - Err short-circuits", () => (
    let result := res.and_then(.Err("e"), (x) => .Ok(x + 1));
    t.expect(result).toBe(.Err("e"))
  ));
  t.it("or_else - Err calls function", () => (
    let result := res.or_else(.Err("e"), (_ : String) => .Ok(0));
    t.expect(result).toBe(.Ok(0))
  ));
  t.it("or_else - Ok passes through", () => (
    let result := res.or_else(.Ok(5), (_ : String) => .Ok(0));
    t.expect(result).toBe(.Ok(5))
  ));
  t.it("flat_map - Ok applies function", () => (
    let result := res.flat_map(.Ok(3), (x) => .Ok(x * 3));
    t.expect(result).toBe(.Ok(9))
  ));
  t.it("flat_map - Err passes through", () => (
    let result := res.flat_map(.Err("fail"), (x : String) => .Ok(x));
    t.expect(result).toBe(.Err("fail"))
  ));
  t.it("flatten - Ok(Ok(v)) unwraps to Ok(v)", () => (
    let result := res.flatten(.Ok(.Ok(42)));
    t.expect(result).toBe(.Ok(42))
  ));
  t.it("flatten - Ok(Err(e)) unwraps to Err(e)", () => (
    let result := res.flatten(.Ok(.Err("inner")));
    t.expect(result).toBe(.Err("inner"))
  ));
  t.it("flatten - Err passes through", () => (
    let result := res.flatten(.Err("outer"));
    t.expect(result).toBe(.Err("outer"))
  ));
  t.it("map_both - Ok applies ok_fn", () => (
    let result := res.map_both(.Ok(2), (x) => x + 10, (e) => e + "!");
    t.expect(result).toBe(.Ok(12))
  ));
  t.it("map_both - Err applies err_fn", () => (
    let result := res.map_both(.Err("bad"), (x) => x + 10, (e) => e + "!");
    t.expect(result).toBe(.Err("bad!"))
  ));
  t.it("to_option - Ok becomes Some", () => (
    let result := res.to_option(.Ok(5));
    t.expect(result).toBe(.Some(5))
  ));
  t.it("to_option - Err becomes None", () => (
    let result := res.to_option(.Err("gone"));
    t.expect(result).toBe(.None)
  ))
));

t.run();
