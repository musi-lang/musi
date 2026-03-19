//! Fixed-capacity ring buffer (circular queue).

import "@std/rt" as rt;

/// Fixed-capacity circular buffer that overwrites oldest elements when full.
export let RingBuffer ['a] := record { data : []'a; head : Int; count : Int; cap : Int };

/// Create an empty ring buffer with the given capacity, using `zero` as the initial fill value.
export let new ['a] := (capacity, zero) => (
  let mut arr := [];
  let fill := (i) => (
    () if i >= capacity
    | (rt.arr_push(arr, zero); fill(i + 1)) if _
  );
  fill(0);
  .{ data := arr, head := 0, count := 0, cap := capacity }
);

let wrap := (idx, cap) => idx - (cap if idx >= cap | 0 if _);

/// Push a value into the buffer, overwriting the oldest element if full.
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

/// Remove and return the oldest value, or `.None` if empty.
export let pop ['a] := (rb) => (
  .None if rb.count = 0
  | (
    let val := rb.data.[rb.head];
    rb.head <- wrap(rb.head + 1, rb.cap);
    rb.count <- rb.count - 1;
    .Some(val)
  ) if _
);

/// Return the oldest value without removing it, or `.None` if empty.
export let peek ['a] := (rb) => (
  .None if rb.count = 0
  | .Some(rb.data.[rb.head]) if _
);

/// Return the number of elements currently in the buffer.
export let len ['a] := (rb) => rb.count;

/// Return `.True` if the buffer contains no elements.
export let is_empty ['a] := (rb) => rb.count = 0;

/// Return `.True` if the buffer has reached its capacity.
export let is_full ['a] := (rb) => rb.count = rb.cap;
