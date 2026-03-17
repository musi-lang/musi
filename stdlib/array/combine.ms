//! Array combination operations.

import "@std/rt" as rt;

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
