//! String search utilities.

import "@std/rt" as rt;

/// Return whether a string contains a given substring.
export let contains := (s, sub) => rt.str_contains(s, sub);

/// Count the number of non-overlapping occurrences of a substring.
export let count_occurrences := (s, sub) => (
  let sub_len := rt.str_len(sub);
  0 if sub_len = 0
  | (
    let str_len := rt.str_len(s);
    let step := (pos, acc) => (
      acc if pos > str_len - sub_len
      | (
        let idx := rt.str_index_of(rt.str_slice(s, pos, str_len), sub);
        acc if idx = -1
        | step(pos + idx + sub_len, acc + 1) if _
      ) if _
    );
    step(0, 0)
  ) if _
);

/// Return the character index of the last occurrence of sub, or -1 if not found.
export let last_index_of := (s, sub) => (
  let sub_len := rt.str_len(sub);
  -1 if sub_len = 0
  | (
    let str_len := rt.str_len(s);
    let step := (pos, last) => (
      last if pos > str_len - sub_len
      | (
        let idx := rt.str_index_of(rt.str_slice(s, pos, str_len), sub);
        last if idx = -1
        | step(pos + idx + sub_len, pos + idx) if _
      ) if _
    );
    step(0, -1)
  ) if _
);
