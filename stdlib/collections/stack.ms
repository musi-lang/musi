//! LIFO stack backed by an array.

import "musi:core" as core;

export let Stack ['a] := record { data : [] 'a };

export let new ['a] := () => .{ data := [] };

export let push ['a] := (s, val) => core.arr_push(s.data, val);

export let pop ['a] := (s) => core.arr_pop(s.data);

export let peek ['a] := (s) => (
  let len := core.arr_len(s.data);
  (.Some(s.data.[len - 1]) if len > 0
  | .None if _)
);

export let len ['a] := (s) => core.arr_len(s.data);

export let is_empty ['a] := (s) => core.arr_len(s.data) = 0;
