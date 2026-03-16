//! String join utilities.

import "@std/rt" as rt;

export let join := (arr, sep) => (
  let len := rt.arr_len(arr);
  "" if len = 0
  | (
    let step := (acc, i) => (
      acc if i >= len
      | step(acc + sep + arr.[i], i + 1) if _
    );
    step(arr.[0], 1)
  ) if _
);
