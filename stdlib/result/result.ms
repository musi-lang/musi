//! Result type and combinators.

/// A value that is either Ok (success) or Err (failure).
#[lang := "Result"]
export let Result ['a, 'e] := choice { Ok of 'a + Err of 'e };

/// Return whether the result is Ok.
export let is_ok ['a, 'e] := (res) =>
  match res (
    .Ok(_) => .True
    | .Err(_) => .False
  );

/// Return whether the result is Err.
export let is_err ['a, 'e] := (res) =>
  match res (
    .Ok(_) => .False
    | .Err(_) => .True
  );

/// Apply f to the Ok value, leaving Err unchanged.
export let map_ok ['a, 'b, 'e] := (res, f) =>
  match res (
    .Ok(v) => .Ok(f(v))
    | .Err(e) => .Err(e)
  );

/// Apply f to the Err value, leaving Ok unchanged.
export let map_err ['a, 'e, 'f] := (res, f) =>
  match res (
    .Ok(v) => .Ok(v)
    | .Err(e) => .Err(f(e))
  );

/// Return the Ok value, or a fallback if Err.
export let unwrap_or ['a, 'e] := (res, default) =>
  match res (
    .Ok(v) => v
    | .Err(_) => default
  );

/// Return the Ok value, or call f on the error to produce a fallback.
export let unwrap_or_else ['a, 'e] := (res, f) =>
  match res (
    .Ok(v) => v
    | .Err(e) => f(e)
  );

/// Chain a fallible operation over the Ok value (flatMap).
export let and_then ['a, 'b, 'e] := (res, f) =>
  match res (
    .Ok(v) => f(v)
    | .Err(e) => .Err(e)
  );

/// Return the result if Ok, otherwise call f on the error to produce a fallback result.
export let or_else ['a, 'e, 'f] := (res, f) =>
  match res (
    .Ok(v) => .Ok(v)
    | .Err(e) => f(e)
  );

/// Apply f to the Ok value and return its result directly (alias for and_then).
export let flat_map ['a, 'b, 'e] := (res, f) =>
  match res (
    .Ok(v) => f(v)
    | .Err(e) => .Err(e)
  );

/// Collapse a Result of Result into a single Result.
export let flatten ['a, 'e] := (res) =>
  match res (
    .Ok(inner) => inner
    | .Err(e) => .Err(e)
  );

/// Apply ok_fn to Ok or err_fn to Err, returning the transformed result.
export let map_both ['a, 'b, 'e, 'f] := (res, ok_fn, err_fn) =>
  match res (
    .Ok(v) => .Ok(ok_fn(v))
    | .Err(e) => .Err(err_fn(e))
  );

/// Convert a Result to an Option, discarding the error.
export let to_option ['a, 'e] := (res) =>
  match res (
    .Ok(v) => .Some(v)
    | .Err(_) => .None
  );
