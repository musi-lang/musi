//! String splitting utilities.

import "@std/rt" as rt;

/// Split a string into lines on newline characters.
export let lines := (s) => rt.str_split(s, "\n");

/// Split a string into words on whitespace, ignoring empty segments.
export let words := (s) => (
  let raw := rt.str_split(rt.str_trim(s), " ");
  let len := rt.arr_len(raw);
  let mut result := [];
  let step := (i) => (
    () if i >= len
    | (
      (rt.arr_push(result, raw.[i]); step(i + 1)) if rt.str_len(raw.[i]) > 0
      | step(i + 1) if _
    ) if _
  );
  step(0);
  result
);
