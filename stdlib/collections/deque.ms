//! Double-ended queue - two-array implementation.
//!
//! `front` stores elements in reverse order: the logical front element is at
//! the end of the array, so `arr_pop` is O(1). `back` stores elements in
//! forward order: the logical back element is at the end of the array.
//!
//! When one side is exhausted, the other is reversed into it (amortised O(1)).

import "@std/rt" as rt;

/// Two-array double-ended queue.
export let Deque ['a] := record { front : []'a; back : []'a };

/// Create an empty deque.
export let new ['a] := () => .{ front := [], back := [] };

/// Push a value onto the front of the deque.
export let push_front ['a] := (d, val) => rt.arr_push(d.front, val);

/// Push a value onto the back of the deque.
export let push_back ['a] := (d, val) => rt.arr_push(d.back, val);

/// Remove and return the front value, or `.None` if empty.
export let pop_front ['a] := (mut d) => (
  let flen := rt.arr_len(d.front);
  let blen := rt.arr_len(d.back);
  (
    .Some(rt.arr_pop(d.front)) if flen > 0
    | (
        rt.arr_reverse(d.back);
        d.front <- d.back;
        d.back <- [];
        .Some(rt.arr_pop(d.front))
      ) if blen > 0
    | .None if _
  )
);

/// Remove and return the back value, or `.None` if empty.
export let pop_back ['a] := (mut d) => (
  let blen := rt.arr_len(d.back);
  let flen := rt.arr_len(d.front);
  (
    .Some(rt.arr_pop(d.back)) if blen > 0
    | (
        rt.arr_reverse(d.front);
        d.back <- d.front;
        d.front <- [];
        .Some(rt.arr_pop(d.back))
      ) if flen > 0
    | .None if _
  )
);

/// Return the front value without removing it, or `.None` if empty.
export let peek_front ['a] := (d) => (
  let flen := rt.arr_len(d.front);
  let blen := rt.arr_len(d.back);
  (
    .Some(d.front.[flen - 1]) if flen > 0
    | .Some(d.back.[0]) if blen > 0
    | .None if _
  )
);

/// Return the back value without removing it, or `.None` if empty.
export let peek_back ['a] := (d) => (
  let blen := rt.arr_len(d.back);
  let flen := rt.arr_len(d.front);
  (
    .Some(d.back.[blen - 1]) if blen > 0
    | .Some(d.front.[0]) if flen > 0
    | .None if _
  )
);

/// Return the total number of elements in the deque.
export let len ['a] := (d) => rt.arr_len(d.front) + rt.arr_len(d.back);

/// Return `.True` if the deque contains no elements.
export let is_empty ['a] := (d) =>
  rt.arr_len(d.front) = 0 and rt.arr_len(d.back) = 0;
