//! Array aggregation operations.

import "@std/rt" as rt;

export let min_of ['a] := (arr) => (
  let len := rt.arr_len(arr);
  (
    let step := (i, cur) => (
      (step(i + 1, arr.[i]) if arr.[i] < cur | step(i + 1, cur) if _) if i < len
      | .Some(cur) if _
    );
    step(1, arr.[0])
  ) if len > 0
  | .None if _
);

export let max_of ['a] := (arr) => (
  let len := rt.arr_len(arr);
  (
    let step := (i, cur) => (
      (step(i + 1, arr.[i]) if arr.[i] > cur | step(i + 1, cur) if _) if i < len
      | .Some(cur) if _
    );
    step(1, arr.[0])
  ) if len > 0
  | .None if _
);

export let count_by ['a] := (arr, pred) => (
  let len := rt.arr_len(arr);
  let step := (i, acc) => (
    (step(i + 1, acc + 1) if pred(arr.[i]) | step(i + 1, acc) if _) if i < len
    | acc if _
  );
  step(0, 0)
);

export let all_arr ['a] := (arr, pred) => (
  let len := rt.arr_len(arr);
  let step := (i) => (
    (step(i + 1) if pred(arr.[i]) | .False if _) if i < len
    | .True if _
  );
  step(0)
);

export let any_arr ['a] := (arr, pred) => (
  let len := rt.arr_len(arr);
  let step := (i) => (
    (.True if pred(arr.[i]) | step(i + 1) if _) if i < len
    | .False if _
  );
  step(0)
);

export let sum_arr := (arr) => (
  let len := rt.arr_len(arr);
  let step := (i, acc) => (
    step(i + 1, acc + arr.[i]) if i < len
    | acc if _
  );
  step(0, 0)
);
