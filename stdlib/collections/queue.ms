//! FIFO queue - two-stack (front/back array) implementation.
//!
//! Enqueue appends to `back`. Dequeue pops from `front`; when `front` is
//! empty, `back` is reversed into `front`.

import "@std/rt" as rt;

/// Two-array FIFO queue.
export let Queue ['a] := record { front : []'a; back : []'a };

/// Create an empty queue.
export let new ['a] := () => .{ front := [], back := [] };

/// Add a value to the back of the queue.
export let enqueue ['a] := (q, val) => rt.arr_push(q.back, val);

/// Remove and return the front value, or `.None` if empty.
export let dequeue ['a] := (mut q) => (
  let flen := rt.arr_len(q.front);
  let blen := rt.arr_len(q.back);
  (
    .Some(rt.arr_pop(q.front)) if flen > 0
    | (
      rt.arr_reverse(q.back);
      q.front <- q.back;
      q.back <- [];
      .Some(rt.arr_pop(q.front))
    ) if blen > 0
    | .None if _
  )
);

/// Return the total number of elements in the queue.
export let len ['a] := (q) => rt.arr_len(q.front) + rt.arr_len(q.back);

/// Return `.True` if the queue contains no elements.
export let is_empty ['a] := (q) =>
  rt.arr_len(q.front) = 0 and rt.arr_len(q.back) = 0;
