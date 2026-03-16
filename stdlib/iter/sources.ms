//! Iterator source constructors.

import "@std/rt" as rt;

export let Iter ['a] := record { next : () -> ?'a };

export let from_array ['a] := (arr) => (
  let mut i := 0;
  let len := rt.arr_len(arr);
  .{ next := () => (
    (let idx := i; i <- i + 1; .Some(arr.[idx])) if i < len
    | .None if _
  ) }
);

export let range := (start, end) => (
  let mut cur := start;
  .{ next := () => (
    (let v := cur; cur <- cur + 1; .Some(v)) if cur < end
    | .None if _
  ) }
);

export let repeat ['a] := (value) =>
  .{ next := () => .Some(value) };

export let empty ['a] := () =>
  .{ next := () => .None };
