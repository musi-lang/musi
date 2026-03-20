//! Fluent matcher API for test assertions.

import "@std/rt" as rt;
import "./runner" as runner;

/// Returns a matcher object with assertion methods for the given actual value.
export let expect := (actual : Any) => .{
  toBe := (expected : Any) => (
    () if actual = expected
    | runner.fail("expected " + rt.show(expected) + ", got " + rt.show(actual)) if _
  ),
  toEqual := (expected : Any) => (
    () if actual = expected
    | runner.fail("expected " + rt.show(expected) + ", got " + rt.show(actual)) if _
  ),
  toBeTruthy := () => (
    () if actual
    | runner.fail("expected truthy, got " + rt.show(actual)) if _
  ),
  toBeFalsy := () => (
    () if not actual
    | runner.fail("expected falsy, got " + rt.show(actual)) if _
  ),
  toBeNone := () => (
    match actual (
      .None => ()
      | _ => runner.fail("expected .None, got " + rt.show(actual))
    )
  ),
  toBeSome := () => (
    match actual (
      .Some(_) => ()
      | _ => runner.fail("expected .Some(_), got " + rt.show(actual))
    )
  ),
  toBeOk := () => (
    match actual (
      .Ok(_) => ()
      | _ => runner.fail("expected .Ok(_), got " + rt.show(actual))
    )
  ),
  toBeErr := () => (
    match actual (
      .Err(_) => ()
      | _ => runner.fail("expected .Err(_), got " + rt.show(actual))
    )
  ),
  toBeGreaterThan := (n : Any) => (
    () if actual > n
    | runner.fail("expected " + rt.show(actual) + " > " + rt.show(n)) if _
  ),
  toBeLessThan := (n : Any) => (
    () if actual < n
    | runner.fail("expected " + rt.show(actual) + " < " + rt.show(n)) if _
  ),
  toContain := (item : Any) => (
    () if rt.arr_contains(actual, item)
    | runner.fail("expected " + rt.show(actual) + " to contain " + rt.show(item)) if _
  ),
  toHaveLength := (n : Int) => (
    let len := rt.arr_len(actual);
    () if len = n
    | runner.fail("expected length " + rt.show(n) + ", got " + rt.show(len)) if _
  ),
  toMatch := (substr : String) => (
    () if rt.str_contains(actual, substr)
    | runner.fail("expected " + rt.show(actual) + " to contain \"" + substr + "\"") if _
  ),
  not := .{
    toBe := (expected : Any) => (
      () if not (actual = expected)
      | runner.fail("expected not " + rt.show(expected) + ", got " + rt.show(actual)) if _
    ),
    toEqual := (expected : Any) => (
      () if not (actual = expected)
      | runner.fail("expected not " + rt.show(expected) + ", got " + rt.show(actual)) if _
    ),
    toBeTruthy := () => (
      () if not actual
      | runner.fail("expected not truthy, got " + rt.show(actual)) if _
    ),
    toBeFalsy := () => (
      () if actual
      | runner.fail("expected not falsy, got " + rt.show(actual)) if _
    ),
    toBeNone := () => (
      match actual (
        .None => runner.fail("expected not .None")
        | _ => ()
      )
    ),
    toBeSome := () => (
      match actual (
        .Some(_) => runner.fail("expected not .Some(_), got " + rt.show(actual))
        | _ => ()
      )
    ),
    toBeOk := () => (
      match actual (
        .Ok(_) => runner.fail("expected not .Ok(_), got " + rt.show(actual))
        | _ => ()
      )
    ),
    toBeErr := () => (
      match actual (
        .Err(_) => runner.fail("expected not .Err(_), got " + rt.show(actual))
        | _ => ()
      )
    ),
    toContain := (item : Any) => (
      () if not rt.arr_contains(actual, item)
      | runner.fail("expected " + rt.show(actual) + " not to contain " + rt.show(item)) if _
    ),
    toMatch := (substr : String) => (
      () if not rt.str_contains(actual, substr)
      | runner.fail("expected " + rt.show(actual) + " not to contain \"" + substr + "\"") if _
    ),
  },
};
