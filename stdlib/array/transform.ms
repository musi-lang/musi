//! Array transformation operations.

import "@std/rt" as rt;

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
