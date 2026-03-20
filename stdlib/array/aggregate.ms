//! Array aggregation operations.

import "@std/rt" as rt;

/// Return the minimum element, or `.None` if the array is empty.
export let min_of ['a] := (arr) => (
  let len := rt.arr_len(arr);
  (
    let step := (i, cur : 'a) => (
      (step(i + 1, arr.[i]) if arr.[i] < cur | step(i + 1, cur) if _) if i < len
      | .Some(cur) if _
    );
    step(1, arr.[0])
  ) if len > 0
  | .None if _
);

/// Return the maximum element, or `.None` if the array is empty.
export let max_of ['a] := (arr) => (
  let len := rt.arr_len(arr);
  (
    let step := (i, cur : 'a) => (
      (step(i + 1, arr.[i]) if arr.[i] > cur | step(i + 1, cur) if _) if i < len
      | .Some(cur) if _
    );
    step(1, arr.[0])
  ) if len > 0
  | .None if _
);

/// Return the number of elements satisfying `pred`.
export let count_by ['a] := (arr, pred) => (
  let len := rt.arr_len(arr);
  let step := (i, acc) => (
    (step(i + 1, acc + 1) if pred(arr.[i]) | step(i + 1, acc) if _) if i < len
    | acc if _
  );
  step(0, 0)
);

/// Return `.True` if every element satisfies `pred`.
export let all_arr ['a] := (arr, pred) => (
  let len := rt.arr_len(arr);
  let step := (i) => (
    (step(i + 1) if pred(arr.[i]) | .False if _) if i < len
    | .True if _
  );
  step(0)
);

/// Return `.True` if at least one element satisfies `pred`.
export let any_arr ['a] := (arr, pred) => (
  let len := rt.arr_len(arr);
  let step := (i) => (
    (.True if pred(arr.[i]) | step(i + 1) if _) if i < len
    | .False if _
  );
  step(0)
);

/// Reduce the array left-to-right using `f`, starting from `init`.
export let fold_arr ['a, 'b] := (arr, init : 'b, f) => (
  let len := rt.arr_len(arr);
  let step := (i, acc : 'b) => (
    step(i + 1, f(acc, arr.[i])) if i < len
    | acc if _
  );
  step(0, init)
);

/// Reduce a non-empty array left-to-right using `f`, or return `.None`.
export let reduce_arr ['a] := (arr, f) => (
  let len := rt.arr_len(arr);
  (
    let step := (i, acc : 'a) => (
      step(i + 1, f(acc, arr.[i])) if i < len
      | .Some(acc) if _
    );
    step(1, arr.[0])
  ) if len > 0
  | .None if _
);

/// Return the sum of all elements.
export let sum_arr := (arr) => (
  let len := rt.arr_len(arr);
  let step := (i, acc) => (
    step(i + 1, acc + arr.[i]) if i < len
    | acc if _
  );
  step(0, 0)
);
