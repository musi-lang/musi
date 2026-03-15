//! FIFO queue — two-stack (front/back array) implementation.
//!
//! Enqueue appends to `back`. Dequeue pops from `front`; when `front` is
//! empty, `back` is reversed into `front`.

foreign "musi:core" (
  let arr_len : [] 'a -> Int;
  let arr_push : [] 'a -> 'a -> Unit;
  let arr_pop : [] 'a -> ?'a;
  let arr_reverse : [] 'a -> [] 'a;
);

export let Queue ['a] := record { front : [] 'a; back : [] 'a };

export let new ['a] := () => .{ front := [], back := [] };

export let enqueue ['a] := (q, val) => arr_push(q.back, val);

export let dequeue ['a] := (q) => (
  let flen := arr_len(q.front);
  let blen := arr_len(q.back);
  (
    arr_pop(q.front) if flen > 0
    | (
      let reversed := arr_reverse(q.back);
      q.front <- reversed;
      q.back <- [];
      arr_pop(q.front)
    ) if blen > 0
    | .None if _
  )
);

export let len ['a] := (q) => arr_len(q.front) + arr_len(q.back);

export let is_empty ['a] := (q) =>
  arr_len(q.front) = 0 and arr_len(q.back) = 0;
