//! LIFO stack backed by an array.

import "@std/rt" as rt;

/// Array-backed LIFO stack.
export let Stack ['a] := record { data : []'a };

/// Create an empty stack.
export let new ['a] := () => .{ data := [] };

/// Push a value onto the top of the stack.
export let push ['a] := (s, val : 'a) => rt.arr_push(s.data, val);

/// Remove and return the top value, or `.None` if empty.
export let pop ['a] := (s) => rt.arr_pop(s.data);

/// Return the top value without removing it, or `.None` if empty.
export let peek ['a] := (s) => (
  let len := rt.arr_len(s.data);
  (.Some(s.data.[len - 1]) if len > 0
  | .None if _)
);

/// Return the number of elements in the stack.
export let len ['a] := (s) => rt.arr_len(s.data);

/// Return `.True` if the stack contains no elements.
export let is_empty ['a] := (s) => rt.arr_len(s.data) = 0;
