//! Array transformation operations.

import "@std/rt" as rt;

/// Remove consecutive duplicate elements from a sorted array.
export let dedup ['a] := (arr) => (
  let len := rt.arr_len(arr);
  let mut result := [];
  let step := (i) => (
    (
      let x : 'a := arr.[i];
      let prev : 'a := arr.[i - 1];
      (rt.arr_push(result, x) if not (x = prev) | () if _);
      step(i + 1)
    ) if i < len
    | result if _
  );
  (
    (
      rt.arr_push(result, arr.[0]);
      step(1)
    ) if len > 0
    | result if _
  )
);

/// Return the first `n` elements of an array.
export let take_arr ['a] := (arr, n) => (
  let len := rt.arr_len(arr);
  let end := (n if n < len | len if _);
  rt.arr_slice(arr, 0, end)
);

/// Return all elements after the first `n` elements of an array.
export let drop_arr ['a] := (arr, n) => (
  let len := rt.arr_len(arr);
  let start := (n if n < len | len if _);
  rt.arr_slice(arr, start, len)
);

/// Map each element to an array and concatenate all results.
export let flat_map_arr ['a, 'b] := (arr, f) => (
  let len := rt.arr_len(arr);
  let mut result := [];
  let step := (i) => (
    (
      let sub := f(arr.[i]);
      let sub_len := rt.arr_len(sub);
      let inner := (j) => (
        (rt.arr_push(result, sub.[j]); inner(j + 1)) if j < sub_len
        | () if _
      );
      inner(0);
      step(i + 1)
    ) if i < len
    | result if _
  );
  step(0)
);

/// Apply `f` to each element and return a new array of results.
export let map_arr ['a, 'b] := (arr, f) => (
  let len := rt.arr_len(arr);
  let mut result := [];
  let step := (i) => (
    (rt.arr_push(result, f(arr.[i])); step(i + 1)) if i < len
    | result if _
  );
  step(0)
);

/// Return a new array containing only elements that satisfy `pred`.
export let filter_arr ['a] := (arr, pred) => (
  let len := rt.arr_len(arr);
  let mut result := [];
  let step := (i) => (
    (
      (rt.arr_push(result, arr.[i]) if pred(arr.[i]) | () if _);
      step(i + 1)
    ) if i < len
    | result if _
  );
  step(0)
);

/// Return a new array with elements in reverse order.
export let reverse_copy ['a] := (arr) => (
  let len := rt.arr_len(arr);
  let mut result := [];
  let step := (i) => (
    (rt.arr_push(result, arr.[i]); step(i - 1)) if i >= 0
    | result if _
  );
  (step(len - 1) if len > 0 | result if _)
);

/// Insert `sep` between every pair of adjacent elements.
export let intersperse ['a] := (arr, sep : 'a) => (
  let len := rt.arr_len(arr);
  let mut result := [];
  let step := (i) => (
    (
      (rt.arr_push(result, sep) if i > 0 | () if _);
      rt.arr_push(result, arr.[i]);
      step(i + 1)
    ) if i < len
    | result if _
  );
  step(0)
);

/// Fold `f` left over the array, emitting each intermediate accumulator.
export let scan ['a, 'b] := (arr, init : 'b, f) => (
  let len := rt.arr_len(arr);
  let mut acc : 'b := init;
  let mut result := [];
  let step := (i) => (
    (
      acc <- f(acc, arr.[i]);
      rt.arr_push(result, acc);
      step(i + 1)
    ) if i < len
    | result if _
  );
  step(0)
);
