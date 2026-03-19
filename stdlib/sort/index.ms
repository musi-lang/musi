//! Sorting algorithms.
//!
//! sort_by and merge_sort operate on copies; inputs are never mutated.

import "@std/rt" as rt;

/// Return `.True` if `arr` is sorted according to `cmp`.
export let is_sorted ['a] := (arr, cmp) => (
  let len := rt.arr_len(arr);
  let step := (i) => (
    (.False if cmp(arr.[i], arr.[i - 1]) < 0 | step(i + 1) if _) if i < len
    | .True if _
  );
  (.True if len <= 1 | step(1) if _)
);

let merge := (left, right, cmp) => (
  let ll := rt.arr_len(left);
  let rl := rt.arr_len(right);
  let mut result := [];
  let step := (i, j) => (
    (
      (
        rt.arr_push(result, left.[i]);
        step(i + 1, j)
      ) if cmp(left.[i], right.[j]) <= 0
      | (
        rt.arr_push(result, right.[j]);
        step(i, j + 1)
      ) if _
    ) if i < ll and j < rl
    | (
      (rt.arr_push(result, left.[i]); step(i + 1, j)) if i < ll
      | (rt.arr_push(result, right.[j]); step(i, j + 1)) if j < rl
      | result if _
    ) if _
  );
  step(0, 0)
);

/// Return a new array sorted by `cmp` using merge sort.
export let merge_sort ['a] := (arr, cmp) => (
  let len := rt.arr_len(arr);
  arr if len <= 1
  | (
    let mid := len / 2;
    let left  := merge_sort(rt.arr_slice(arr, 0, mid), cmp);
    let right := merge_sort(rt.arr_slice(arr, mid, len), cmp);
    merge(left, right, cmp)
  ) if _
);

/// Return a new array sorted by `cmp`.
export let sort_by ['a] := (arr, cmp) => merge_sort(arr, cmp);

/// Return a new array sorted by `cmp` using insertion sort (efficient for small or nearly-sorted arrays).
export let insertion_sort ['a] := (arr, cmp) => (
  let len := rt.arr_len(arr);
  let sorted := rt.arr_slice(arr, 0, len);
  let outer := (i) => (
    (
      let key := sorted.[i];
      let inner := (j) => (
        (
          sorted.[j + 1] <- sorted.[j];
          sorted.[j] <- key;
          inner(j - 1)
        ) if j >= 0 and cmp(key, sorted.[j]) < 0
        | () if _
      );
      inner(i - 1);
      outer(i + 1)
    ) if i < len
    | sorted if _
  );
  outer(1)
);
