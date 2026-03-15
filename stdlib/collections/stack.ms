//! LIFO stack backed by an array.

foreign "musi:core" (
  let arr_len : [] 'a -> Int;
  let arr_push : [] 'a -> 'a -> Unit;
  let arr_pop : [] 'a -> ?'a;
);

export let Stack ['a] := record { data : [] 'a };

export let new ['a] := () => .{ data := [] };

export let push ['a] := (s, val) => arr_push(s.data, val);

export let pop ['a] := (s) => arr_pop(s.data);

export let peek ['a] := (s) => (
  let len := arr_len(s.data);
  (.Some(s.data.[len - 1]) if len > 0
  | .None if _)
);

export let len ['a] := (s) => arr_len(s.data);

export let is_empty ['a] := (s) => arr_len(s.data) = 0;
