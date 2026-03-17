//! Array search operations.

import "@std/rt" as rt;

export let first ['a] := (arr) => (
  .Some(arr.[0]) if rt.arr_len(arr) > 0
  | .None if _
);

export let last ['a] := (arr) => (
  let len := rt.arr_len(arr);
  .Some(arr.[len - 1]) if len > 0
  | .None if _
);

export let find_index ['a] := (arr, pred) => (
  let len := rt.arr_len(arr);
  let step := (i) => (
    (.Some(i) if pred(arr.[i]) | step(i + 1) if _) if i < len
    | .None if _
  );
  step(0)
);

export let find_last_index ['a] := (arr, pred) => (
  let len := rt.arr_len(arr);
  let step := (i) => (
    (.Some(i) if pred(arr.[i]) | step(i - 1) if _) if i >= 0
    | .None if _
  );
  (step(len - 1) if len > 0 | .None if _)
);
