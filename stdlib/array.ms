//! Higher-order array operations for Musi.
//!
//! All functions return new arrays and do not mutate their inputs,
//! except where noted (sort_by operates on a copy).

import "../rt" as rt;

export let partition ['a] := (arr, pred) => (
  let mut matching := [];
  let mut rest := [];
  let len := rt.arr_len(arr);
  let step := (i) => (
    (
      let x := arr.[i];
      (rt.arr_push(matching, x) if pred(x) | rt.arr_push(rest, x) if _);
      step(i + 1)
    ) if i < len
    | (matching, rest) if _
  );
  step(0)
);

export let chunk ['a] := (arr, size) => (
  let len := rt.arr_len(arr);
  let mut result := [];
  let step := (i) => (
    (
      let end := (i + size if i + size < len | len if _);
      rt.arr_push(result, rt.arr_slice(arr, i, end));
      step(i + size)
    ) if i < len
    | result if _
  );
  step(0)
);

export let windows ['a] := (arr, size) => (
  let len := rt.arr_len(arr);
  let mut result := [];
  let step := (i) => (
    (
      rt.arr_push(result, rt.arr_slice(arr, i, i + size));
      step(i + 1)
    ) if i + size <= len
    | result if _
  );
  step(0)
);

export let split_at ['a] := (arr, idx) => (
  let len := rt.arr_len(arr);
  let cut := (idx if idx < len | len if _);
  (rt.arr_slice(arr, 0, cut), rt.arr_slice(arr, cut, len))
);

export let distinct ['a] := (arr) => (
  let len := rt.arr_len(arr);
  let mut result := [];
  let step := (i) => (
    (
      let x := arr.[i];
      let prev := arr.[i - 1];
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

export let take_arr ['a] := (arr, n) => (
  let len := rt.arr_len(arr);
  let end := (n if n < len | len if _);
  rt.arr_slice(arr, 0, end)
);

export let drop_arr ['a] := (arr, n) => (
  let len := rt.arr_len(arr);
  let start := (n if n < len | len if _);
  rt.arr_slice(arr, start, len)
);

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

export let zip_arr ['a, 'b] := (a, b) => (
  let len_a := rt.arr_len(a);
  let len_b := rt.arr_len(b);
  let len := (len_a if len_a < len_b | len_b if _);
  let mut result := [];
  let step := (i) => (
    (rt.arr_push(result, (a.[i], b.[i])); step(i + 1)) if i < len
    | result if _
  );
  step(0)
);

export let unzip ['a, 'b] := (arr) => (
  let len := rt.arr_len(arr);
  let mut lefts := [];
  let mut rights := [];
  let step := (i) => (
    (
      let pair := arr.[i];
      rt.arr_push(lefts, pair.0);
      rt.arr_push(rights, pair.1);
      step(i + 1)
    ) if i < len
    | (lefts, rights) if _
  );
  step(0)
);

export let intersperse ['a] := (arr, sep) => (
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

export let scan ['a, 'b] := (arr, init, f) => (
  let len := rt.arr_len(arr);
  let mut acc := init;
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

export let map_arr ['a, 'b] := (arr, f) => (
  let len := rt.arr_len(arr);
  let mut result := [];
  let step := (i) => (
    (rt.arr_push(result, f(arr.[i])); step(i + 1)) if i < len
    | result if _
  );
  step(0)
);

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

export let reverse_copy ['a] := (arr) => (
  let len := rt.arr_len(arr);
  let mut result := [];
  let step := (i) => (
    (rt.arr_push(result, arr.[i]); step(i - 1)) if i >= 0
    | result if _
  );
  (step(len - 1) if len > 0 | result if _)
);

export let sort_by ['a] := (arr, cmp) => (
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
