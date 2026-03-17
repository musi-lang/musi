//! Tests for result type and combinators.

import "@std/testing" as t;
import "@std/result/result" as res;

t.runner.report(t.runner.run_suite([
  ("is_ok - Ok returns true", () => (
    t.assertions.assert_true(res.is_ok(.Ok(1)))
  )),
  ("is_ok - Err returns false", () => (
    t.assertions.assert_false(res.is_ok(.Err("oops")))
  )),
  ("is_err - Err returns true", () => (
    t.assertions.assert_true(res.is_err(.Err("bad")))
  )),
  ("is_err - Ok returns false", () => (
    t.assertions.assert_false(res.is_err(.Ok(42)))
  )),
  ("map_ok - Ok applies function", () => (
    let result := res.map_ok(.Ok(5), (x) => x * 2);
    t.assertions.assert_eq(result, .Ok(10))
  )),
  ("map_ok - Err passes through unchanged", () => (
    let result := res.map_ok(.Err("e"), (x) => x * 2);
    t.assertions.assert_eq(result, .Err("e"))
  )),
  ("map_err - Err applies function", () => (
    let result := res.map_err(.Err("bad"), (e) => e + "!");
    t.assertions.assert_eq(result, .Err("bad!"))
  )),
  ("map_err - Ok passes through unchanged", () => (
    let result := res.map_err(.Ok(3), (e) => e + "!");
    t.assertions.assert_eq(result, .Ok(3))
  )),
  ("unwrap_or - Ok returns inner value", () => (
    let result := res.unwrap_or(.Ok(7), 0);
    t.assertions.assert_eq(result, 7)
  )),
  ("unwrap_or - Err returns default", () => (
    let result := res.unwrap_or(.Err("fail"), 0);
    t.assertions.assert_eq(result, 0)
  )),
  ("unwrap_or_else - Ok returns inner value", () => (
    let result := res.unwrap_or_else(.Ok(9), (_) => 0);
    t.assertions.assert_eq(result, 9)
  )),
  ("unwrap_or_else - Err calls function with error", () => (
    let result := res.unwrap_or_else(.Err("x"), (_) => 99);
    t.assertions.assert_eq(result, 99)
  )),
  ("and_then - Ok applies function returning Ok", () => (
    let result := res.and_then(.Ok(4), (x) => .Ok(x + 1));
    t.assertions.assert_eq(result, .Ok(5))
  )),
  ("and_then - Ok applies function returning Err", () => (
    let result := res.and_then(.Ok(4), (_) => .Err("nope"));
    t.assertions.assert_eq(result, .Err("nope"))
  )),
  ("and_then - Err short-circuits", () => (
    let result := res.and_then(.Err("e"), (x) => .Ok(x + 1));
    t.assertions.assert_eq(result, .Err("e"))
  )),
  ("or_else - Err calls function", () => (
    let result := res.or_else(.Err("e"), (_) => .Ok(0));
    t.assertions.assert_eq(result, .Ok(0))
  )),
  ("or_else - Ok passes through", () => (
    let result := res.or_else(.Ok(5), (_) => .Ok(0));
    t.assertions.assert_eq(result, .Ok(5))
  )),
  ("flat_map - Ok applies function", () => (
    let result := res.flat_map(.Ok(3), (x) => .Ok(x * 3));
    t.assertions.assert_eq(result, .Ok(9))
  )),
  ("flat_map - Err passes through", () => (
    let result := res.flat_map(.Err("fail"), (x) => .Ok(x));
    t.assertions.assert_eq(result, .Err("fail"))
  )),
  ("flatten - Ok(Ok(v)) unwraps to Ok(v)", () => (
    let result := res.flatten(.Ok(.Ok(42)));
    t.assertions.assert_eq(result, .Ok(42))
  )),
  ("flatten - Ok(Err(e)) unwraps to Err(e)", () => (
    let result := res.flatten(.Ok(.Err("inner")));
    t.assertions.assert_eq(result, .Err("inner"))
  )),
  ("flatten - Err passes through", () => (
    let result := res.flatten(.Err("outer"));
    t.assertions.assert_eq(result, .Err("outer"))
  )),
  ("map_both - Ok applies ok_fn", () => (
    let result := res.map_both(.Ok(2), (x) => x + 10, (e) => e + "!");
    t.assertions.assert_eq(result, .Ok(12))
  )),
  ("map_both - Err applies err_fn", () => (
    let result := res.map_both(.Err("bad"), (x) => x + 10, (e) => e + "!");
    t.assertions.assert_eq(result, .Err("bad!"))
  )),
  ("to_option - Ok becomes Some", () => (
    let result := res.to_option(.Ok(5));
    t.assertions.assert_eq(result, .Some(5))
  )),
  ("to_option - Err becomes None", () => (
    let result := res.to_option(.Err("gone"));
    t.assertions.assert_eq(result, .None)
  )),
]));
