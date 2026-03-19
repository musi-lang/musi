//! Fixed-capacity ring buffer (circular queue).

import "@std/rt" as rt;

export let RingBuffer ['a] := record { data : []'a; head : Int; count : Int; cap : Int };

export let new ['a] := (capacity) => (
  let mut arr := [];
  let fill := (i) => (
    () if i >= capacity
    | (rt.arr_push(arr, 0); fill(i + 1)) if _
  );
  fill(0);
  .{ data := arr, head := 0, count := 0, cap := capacity }
);

let wrap := (idx, cap) => idx - (cap if idx >= cap | 0 if _);

export let push ['a] := (rb, val) => (
  (
    let slot := wrap(rb.head + rb.count, rb.cap);
    rb.data.[slot] <- val;
    rb.count <- rb.count + 1
  ) if rb.count < rb.cap
  | (
    rb.data.[rb.head] <- val;
    rb.head <- wrap(rb.head + 1, rb.cap)
  ) if _
);

export let pop ['a] := (rb) => (
  .None if rb.count = 0
  | (
    let val := rb.data.[rb.head];
    rb.head <- wrap(rb.head + 1, rb.cap);
    rb.count <- rb.count - 1;
    .Some(val)
  ) if _
);

export let peek ['a] := (rb) => (
  .None if rb.count = 0
  | .Some(rb.data.[rb.head]) if _
);

export let len ['a] := (rb) => rb.count;

export let is_empty ['a] := (rb) => rb.count = 0;

export let is_full ['a] := (rb) => rb.count = rb.cap;
