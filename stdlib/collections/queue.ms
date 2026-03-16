//! FIFO queue — two-stack (front/back array) implementation.
//!
//! Enqueue appends to `back`. Dequeue pops from `front`; when `front` is
//! empty, `back` is reversed into `front`.

import "@std/rt" as rt;

export let Queue ['a] := record { front : [] 'a; back : [] 'a };

export let new ['a] := () => .{ front := [], back := [] };

export let enqueue ['a] := (q, val) => rt.arr_push(q.back, val);

export let dequeue ['a] := (q) => (
  let flen := rt.arr_len(q.front);
  let blen := rt.arr_len(q.back);
  (
    rt.arr_pop(q.front) if flen > 0
    | (
      rt.arr_reverse(q.back);
      q.front <- q.back;
      q.back <- [];
      rt.arr_pop(q.front)
    ) if blen > 0
    | .None if _
  )
);

export let len ['a] := (q) => rt.arr_len(q.front) + rt.arr_len(q.back);

export let is_empty ['a] := (q) =>
  rt.arr_len(q.front) = 0 and rt.arr_len(q.back) = 0;
