//! Option type and combinators.

/// An optional value: Some holds a value, None represents absence.
#[lang := "Option"]
export let Option ['a] := choice { None + Some of 'a };

/// Return whether the option holds a value.
export let is_some ['a] := (opt : ?'a) =>
  match opt (
    .Some(_) => .True
    | .None => .False
  );

/// Return whether the option is empty.
export let is_none ['a] := (opt : ?'a) =>
  match opt (
    .Some(_) => .False
    | .None => .True
  );

/// Return whether the option contains a specific value.
export let contains ['a] := (opt : ?'a, val : 'a) =>
  match opt (
    .Some(v : 'a) => v = val
    | .None => .False
  );

/// Extract the value, propagating an error if the option is None.
export let unwrap ['a] := (opt : ?'a) =>
  match opt (
    .Some(v : 'a) => v
    | .None => .Err("unwrap called on None")?
  );

/// Return the contained value, or a fallback if None.
export let unwrap_or ['a] := (opt : ?'a, default : 'a) =>
  match opt (
    .Some(v : 'a) => v
    | .None => default
  );

/// Return the contained value, or call f to produce a fallback if None.
export let unwrap_or_else ['a] := (opt : ?'a, f) =>
  match opt (
    .Some(v : 'a) => v
    | .None => f()
  );

/// Apply f to the contained value and wrap the result in Some, or propagate None.
export let map ['a, 'b] := (opt : ?'a, f) =>
  match opt (
    .Some(v : 'a) => .Some(f(v))
    | .None => .None
  );

/// Chain a fallible operation over the contained value (flatMap).
export let and_then ['a, 'b] := (opt : ?'a, f) =>
  match opt (
    .Some(v : 'a) => f(v)
    | .None => .None
  );

/// Return None if the contained value does not satisfy pred.
export let filter ['a] := (opt : ?'a, pred) =>
  match opt (
    .Some(v : 'a) => (.Some(v) if pred(v) | .None if _)
    | .None => .None
  );

/// Apply f to the value if Some, or return a default if None.
export let map_or ['a, 'b] := (opt : ?'a, default : 'b, f) =>
  match opt (
    .Some(v : 'a) => f(v)
    | .None => default
  );

/// Call f for its side effect if the option is Some, then return the option unchanged.
export let inspect ['a] := (opt : ?'a, f) =>
  match opt (
    .Some(v : 'a) => (f(v); .Some(v))
    | .None => .None
  );

/// Collapse an Option of Option into a single Option.
export let flatten ['a] := (opt : ?(?'a)) =>
  match opt (
    .Some(inner : ?'a) => inner
    | .None => .None
  );

/// Return the option if Some, otherwise call f to produce a fallback option.
export let or_else ['a] := (opt : ?'a, f) =>
  match opt (
    .Some(v : 'a) => .Some(v)
    | .None => f()
  );

/// Return b if a is Some, otherwise None (logical and).
export let and_opt ['a, 'b] := (a : ?'a, b : ?'b) =>
  match a (
    .Some(_) => b
    | .None => .None
  );

/// Return a if Some, otherwise b (logical or).
export let or_opt ['a] := (a : ?'a, b : ?'a) =>
  match a (
    .Some(v : 'a) => .Some(v)
    | .None => b
  );

/// Return Some only if exactly one of a or b is Some (logical xor).
export let xor_opt ['a] := (a : ?'a, b : ?'a) =>
  match a (
    .Some(v : 'a) => match b (
      .Some(_) => .None
      | .None => .Some(v)
    )
    | .None => b
  );

/// Pair the values of two options into a tuple, or return None if either is None.
export let zip ['a, 'b] := (a : ?'a, b : ?'b) =>
  match a (
    .Some(x : 'a) => match b (
      .Some(y : 'b) => .Some((x, y))
      | .None => .None
    )
    | .None => .None
  );

/// Combine the values of two options with f, or return None if either is None.
export let zip_with ['a, 'b, 'c] := (a : ?'a, b : ?'b, f) =>
  match a (
    .Some(x : 'a) => match b (
      .Some(y : 'b) => .Some(f(x, y))
      | .None => .None
    )
    | .None => .None
  );

/// Convert an option to a Result, using err as the error if None.
export let to_result ['a, 'e] := (opt : ?'a, err : 'e) =>
  match opt (
    .Some(v : 'a) => .Ok(v)
    | .None => .Err(err)
  );

/// Convert an option to a single-element array, or an empty array if None.
export let to_array ['a] := (opt : ?'a) =>
  match opt (
    .Some(v : 'a) => [v]
    | .None => []
  );
