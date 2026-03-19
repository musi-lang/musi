//! Array search operations.

import "@std/rt" as rt;

/// Return the first element, or `.None` if the array is empty.
export let first ['a] := (arr) => (
  .Some(arr.[0]) if rt.arr_len(arr) > 0
  | .None if _
);

/// Return the last element, or `.None` if the array is empty.
export let last ['a] := (arr) => (
  let len := rt.arr_len(arr);
  .Some(arr.[len - 1]) if len > 0
  | .None if _
);

/// Return the index of the first element satisfying `pred`, or `.None`.
export let find_index ['a] := (arr, pred) => (
  let len := rt.arr_len(arr);
  let step := (i) => (
    (.Some(i) if pred(arr.[i]) | step(i + 1) if _) if i < len
    | .None if _
  );
  step(0)
);

/// Return the index of the last element satisfying `pred`, or `.None`.
export let find_last_index ['a] := (arr, pred) => (
  let len := rt.arr_len(arr);
  let step := (i) => (
    (.Some(i) if pred(arr.[i]) | step(i - 1) if _) if i >= 0
    | .None if _
  );
  (step(len - 1) if len > 0 | .None if _)
);
