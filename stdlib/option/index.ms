//! Option type and combinators.

#[lang := "Option"]
export let Option ['a] := choice { None + Some of 'a };

export let is_some ['a] := (opt) =>
  match opt (
    .Some(_) => .True
    | .None => .False
  );

export let is_none ['a] := (opt) =>
  match opt (
    .Some(_) => .False
    | .None => .True
  );

export let contains ['a] := (opt, val) =>
  match opt (
    .Some(v) => v = val
    | .None => .False
  );

export let unwrap ['a] := (opt) =>
  match opt (
    .Some(v) => v
    | .None => .Err("unwrap called on None")?
  );

export let unwrap_or ['a] := (opt, default) =>
  match opt (
    .Some(v) => v
    | .None => default
  );

export let unwrap_or_else ['a] := (opt, f) =>
  match opt (
    .Some(v) => v
    | .None => f()
  );

export let map ['a, 'b] := (opt, f) =>
  match opt (
    .Some(v) => .Some(f(v))
    | .None => .None
  );

export let and_then ['a, 'b] := (opt, f) =>
  match opt (
    .Some(v) => f(v)
    | .None => .None
  );

export let filter ['a] := (opt, pred) =>
  match opt (
    .Some(v) => (.Some(v) if pred(v) | .None if _)
    | .None => .None
  );

export let map_or ['a, 'b] := (opt, default, f) =>
  match opt (
    .Some(v) => f(v)
    | .None => default
  );

export let inspect ['a] := (opt, f) =>
  match opt (
    .Some(v) => (f(v); .Some(v))
    | .None => .None
  );

export let flatten ['a] := (opt) =>
  match opt (
    .Some(inner) => inner
    | .None => .None
  );

export let or_else ['a] := (opt, f) =>
  match opt (
    .Some(v) => .Some(v)
    | .None => f()
  );

export let and_opt ['a, 'b] := (a, b) =>
  match a (
    .Some(_) => b
    | .None => .None
  );

export let or_opt ['a] := (a, b) =>
  match a (
    .Some(v) => .Some(v)
    | .None => b
  );

export let xor_opt ['a] := (a, b) =>
  match a (
    .Some(v) => match b (
      .Some(_) => .None
      | .None => .Some(v)
    )
    | .None => b
  );

export let zip ['a, 'b] := (a, b) =>
  match a (
    .Some(x) => match b (
      .Some(y) => .Some((x, y))
      | .None => .None
    )
    | .None => .None
  );

export let zip_with ['a, 'b, 'c] := (a, b, f) =>
  match a (
    .Some(x) => match b (
      .Some(y) => .Some(f(x, y))
      | .None => .None
    )
    | .None => .None
  );

export let to_result ['a, 'e] := (opt, err) =>
  match opt (
    .Some(v) => .Ok(v)
    | .None => .Err(err)
  );

export let to_array ['a] := (opt) =>
  match opt (
    .Some(v) => [v]
    | .None => []
  );
