//! Assertion functions for Musi tests and contracts.
//!
//! All functions propagate `AssertError` on failure via the caller's
//! `Propagate` instance.  Import this module in test files and wire up
//! a `Propagate AssertError` instance that terminates the test runner.

import "@std/result/propagate";
import "@std/text/show";
import "@std/cmp/ord";

/// Error type propagated by all assertion failures.
export let AssertError := choice { Failed of String };

/// Propagates AssertError with msg if cond is .False.
export let assert := (cond, msg) => (
  () if cond
  | propagate(.Failed(msg)) if _
);

/// Propagates AssertError if cond is .False.
export let assert_true := (cond) => (
  () if cond
  | propagate(.Failed("expected .True, got .False")) if _
);

/// Propagates AssertError if cond is .True.
export let assert_false := (cond) => (
  () if not cond
  | propagate(.Failed("expected .False, got .True")) if _
);

/// Propagates AssertError if actual and expected are not equal.
export let assert_eq ['a] := (actual, expected) => (
  () if actual = expected
  | propagate(.Failed(f"expected {show(expected)}, got {show(actual)}")) if _
);

/// Propagates AssertError if actual and expected are equal.
export let assert_ne ['a] := (actual, expected) => (
  () if not (actual = expected)
  | propagate(.Failed(f"expected values to differ, but both were {show(actual)}")) if _
);

/// Propagates AssertError if a is not strictly less than b.
export let assert_less ['a] := (a, b) => (
  match compare(a, b) (
    .Less => ()
    | .Equal => propagate(.Failed(f"expected {show(a)} < {show(b)}, but they are equal"))
    | .Greater => propagate(.Failed(f"expected {show(a)} < {show(b)}, but got greater"))
  )
);

/// Propagates AssertError if a is not strictly greater than b.
export let assert_greater ['a] := (a, b) => (
  match compare(a, b) (
    .Greater => ()
    | .Equal => propagate(.Failed(f"expected {show(a)} > {show(b)}, but they are equal"))
    | .Less => propagate(.Failed(f"expected {show(a)} > {show(b)}, but got less"))
  )
);

/// Propagates AssertError if opt is .None, otherwise returns the inner value.
export let assert_some ['a] := (opt) =>
  match opt (
    .Some(v) => v
    | .None => propagate(.Failed("expected .Some, got .None"))
  );

/// Propagates AssertError if opt is .Some.
export let assert_none ['a] := (opt) =>
  match opt (
    .None => ()
    | .Some(_) => propagate(.Failed("expected .None, got .Some"))
  );

/// Propagates AssertError if res is .Err, otherwise returns the inner value.
export let assert_ok ['a, 'e] := (res) =>
  match res (
    .Ok(v) => v
    | .Err(_) => propagate(.Failed("expected .Ok, got .Err"))
  );

/// Propagates AssertError if res is .Ok, otherwise returns the inner error.
export let assert_err ['a, 'e] := (res) =>
  match res (
    .Err(e) => e
    | .Ok(_) => propagate(.Failed("expected .Err, got .Ok"))
  );
