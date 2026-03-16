//! Assertion functions for Musi tests and contracts.
//!
//! All functions propagate `AssertError` on failure via the caller's
//! `Propagate` instance.  Import this module in test files and wire up
//! a `Propagate AssertError` instance that terminates the test runner.

import "@std/result/propagate";
import "@std/text/show";
import "@std/cmp/ord";

export let AssertError := choice { Failed of String };

export let assert := (cond, msg) => (
  () if cond
  | propagate(.Failed(msg)) if _
);

export let assert_true := (cond) => (
  () if cond
  | propagate(.Failed("expected .True, got .False")) if _
);

export let assert_false := (cond) => (
  () if not cond
  | propagate(.Failed("expected .False, got .True")) if _
);

export let assert_eq ['a] := (actual, expected) => (
  () if actual = expected
  | propagate(.Failed(f"expected {show(expected)}, got {show(actual)}")) if _
);

export let assert_ne ['a] := (actual, expected) => (
  () if not (actual = expected)
  | propagate(.Failed(f"expected values to differ, but both were {show(actual)}")) if _
);

export let assert_less ['a] := (a, b) => (
  match compare(a, b) (
    .Less => ()
    | .Equal => propagate(.Failed(f"expected {show(a)} < {show(b)}, but they are equal"))
    | .Greater => propagate(.Failed(f"expected {show(a)} < {show(b)}, but got greater"))
  )
);

export let assert_greater ['a] := (a, b) => (
  match compare(a, b) (
    .Greater => ()
    | .Equal => propagate(.Failed(f"expected {show(a)} > {show(b)}, but they are equal"))
    | .Less => propagate(.Failed(f"expected {show(a)} > {show(b)}, but got less"))
  )
);

export let assert_some ['a] := (opt) =>
  match opt (
    .Some(v) => v
    | .None => propagate(.Failed("expected .Some, got .None"))
  );

export let assert_none ['a] := (opt) =>
  match opt (
    .None => ()
    | .Some(_) => propagate(.Failed("expected .None, got .Some"))
  );

export let assert_ok ['a, 'e] := (res) =>
  match res (
    .Ok(v) => v
    | .Err(_) => propagate(.Failed("expected .Ok, got .Err"))
  );

export let assert_err ['a, 'e] := (res) =>
  match res (
    .Err(e) => e
    | .Ok(_) => propagate(.Failed("expected .Err, got .Ok"))
  );
