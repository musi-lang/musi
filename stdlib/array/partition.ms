//! Array partitioning and chunking operations.

import "@std/rt" as rt;

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
