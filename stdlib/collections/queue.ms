//! FIFO queue — two-stack (front/back array) implementation.
//!
//! Enqueue appends to `back`. Dequeue pops from `front`; when `front` is
//! empty, `back` is reversed into `front`.

import "musi:core" as core;

export let Queue ['a] := record { front : [] 'a; back : [] 'a };

export let new ['a] := () => .{ front := [], back := [] };

export let enqueue ['a] := (q, val) => core.arr_push(q.back, val);

export let dequeue ['a] := (q) => (
  let flen := core.arr_len(q.front);
  let blen := core.arr_len(q.back);
  (
    core.arr_pop(q.front) if flen > 0
    | (
      let reversed := core.arr_reverse(q.back);
      q.front <- reversed;
      q.back <- [];
      core.arr_pop(q.front)
    ) if blen > 0
    | .None if _
  )
);

export let len ['a] := (q) => core.arr_len(q.front) + core.arr_len(q.back);

export let is_empty ['a] := (q) =>
  core.arr_len(q.front) = 0 and core.arr_len(q.back) = 0;
