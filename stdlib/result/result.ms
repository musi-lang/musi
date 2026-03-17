//! Result type and combinators.

#[lang := "Result"]
export let Result ['a, 'e] := choice { Ok of 'a + Err of 'e };

export let is_ok ['a, 'e] := (res) =>
  match res (
    .Ok(_) => .True
    | .Err(_) => .False
  );

export let is_err ['a, 'e] := (res) =>
  match res (
    .Ok(_) => .False
    | .Err(_) => .True
  );

export let map_ok ['a, 'b, 'e] := (res, f) =>
  match res (
    .Ok(v) => .Ok(f(v))
    | .Err(e) => .Err(e)
  );

export let map_err ['a, 'e, 'f] := (res, f) =>
  match res (
    .Ok(v) => .Ok(v)
    | .Err(e) => .Err(f(e))
  );

export let unwrap_or ['a, 'e] := (res, default) =>
  match res (
    .Ok(v) => v
    | .Err(_) => default
  );

export let unwrap_or_else ['a, 'e] := (res, f) =>
  match res (
    .Ok(v) => v
    | .Err(e) => f(e)
  );

export let and_then ['a, 'b, 'e] := (res, f) =>
  match res (
    .Ok(v) => f(v)
    | .Err(e) => .Err(e)
  );

export let or_else ['a, 'e, 'f] := (res, f) =>
  match res (
    .Ok(v) => .Ok(v)
    | .Err(e) => f(e)
  );

export let flat_map ['a, 'b, 'e] := (res, f) =>
  match res (
    .Ok(v) => f(v)
    | .Err(e) => .Err(e)
  );

export let flatten ['a, 'e] := (res) =>
  match res (
    .Ok(inner) => inner
    | .Err(e) => .Err(e)
  );

export let map_both ['a, 'b, 'e, 'f] := (res, ok_fn, err_fn) =>
  match res (
    .Ok(v) => .Ok(ok_fn(v))
    | .Err(e) => .Err(err_fn(e))
  );

export let to_option ['a, 'e] := (res) =>
  match res (
    .Ok(v) => .Some(v)
    | .Err(_) => .None
  );
