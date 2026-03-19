//! LIFO stack backed by an array.

import "@std/rt" as rt;

export let Stack ['a] := record { data : []'a };

export let new ['a] := () => .{ data := [] };

export let push ['a] := (s, val) => rt.arr_push(s.data, val);

export let pop ['a] := (s) => rt.arr_pop(s.data);

export let peek ['a] := (s) => (
  let len := rt.arr_len(s.data);
  (.Some(s.data.[len - 1]) if len > 0
  | .None if _)
);

export let len ['a] := (s) => rt.arr_len(s.data);

export let is_empty ['a] := (s) => rt.arr_len(s.data) = 0;
