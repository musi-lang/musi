//! Extended Option and Result combinators for Musi.

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

export let zip_option ['a, 'b] := (a, b) =>
  match a (
    .Some(x) => match b (
      .Some(y) => .Some((x, y))
      | .None => .None
    )
    | .None => .None
  );

export let filter_option ['a] := (opt, pred) =>
  match opt (
    .Some(v) => (.Some(v) if pred(v) | .None if _)
    | .None => .None
  );

export let option_to_result ['a, 'e] := (opt, err) =>
  match opt (
    .Some(v) => .Ok(v)
    | .None => .Err(err)
  );

export let option_to_array ['a] := (opt) =>
  match opt (
    .Some(v) => [v]
    | .None => []
  );

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

export let flatten_result ['a, 'e] := (res) =>
  match res (
    .Ok(inner) => inner
    | .Err(e) => .Err(e)
  );

export let or_else_result ['a, 'e, 'f] := (res, f) =>
  match res (
    .Ok(v) => .Ok(v)
    | .Err(e) => f(e)
  );

export let and_then_result ['a, 'b, 'e] := (res, f) =>
  match res (
    .Ok(v) => f(v)
    | .Err(e) => .Err(e)
  );

export let result_to_option ['a, 'e] := (res) =>
  match res (
    .Ok(v) => .Some(v)
    | .Err(_) => .None
  );

export let unwrap_or_else ['a, 'e] := (res, f) =>
  match res (
    .Ok(v) => v
    | .Err(e) => f(e)
  );

export let map_both ['a, 'b, 'e, 'f] := (res, ok_fn, err_fn) =>
  match res (
    .Ok(v) => .Ok(ok_fn(v))
    | .Err(e) => .Err(err_fn(e))
  );
