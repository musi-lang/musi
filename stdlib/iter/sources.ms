//! Iterator source constructors.

import "@std/rt" as rt;

/// Lazy iterator that yields elements one at a time via `next`.
export let Iter ['a] := record { next : () -> ?'a };

/// Create an iterator that yields each element of `arr` in order.
export let from_array ['a] := (arr) => (
  let mut i := 0;
  let len := rt.arr_len(arr);
  .{ next := () => (
    (let idx := i; i <- i + 1; .Some(arr.[idx])) if i < len
    | .None if _
  ) }
);

/// Create an iterator that yields integers from `start` up to (exclusive) `end`.
export let range := (start, end) => (
  let mut cur := start;
  .{ next := () => (
    (let v := cur; cur <- cur + 1; .Some(v)) if cur < end
    | .None if _
  ) }
);

/// Create an iterator that yields `value` indefinitely.
export let repeat ['a] := (value : 'a) =>
  .{ next := () => .Some(value) };

/// Create an iterator that immediately yields `.None`.
export let empty ['a] := () =>
  .{ next := () => .None };
