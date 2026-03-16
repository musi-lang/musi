//! String padding utilities.

import "@std/rt" as rt;

export let pad_start := (s, target_len, pad_char) => (
  let step := (acc, remaining) => (
    acc if remaining <= 0
    | step(pad_char + acc, remaining - 1) if _
  );
  step(s, target_len - rt.str_len(s))
);

export let pad_end := (s, target_len, pad_char) => (
  let step := (acc, remaining) => (
    acc if remaining <= 0
    | step(acc + pad_char, remaining - 1) if _
  );
  step(s, target_len - rt.str_len(s))
);

export let center := (s, target_len, pad_char) => (
  let deficit := target_len - rt.str_len(s);
  let left_pad := deficit / 2;
  let right_pad := deficit - left_pad;
  let add_left := (acc, remaining) => (
    acc if remaining <= 0
    | add_left(pad_char + acc, remaining - 1) if _
  );
  let add_right := (acc, remaining) => (
    acc if remaining <= 0
    | add_right(acc + pad_char, remaining - 1) if _
  );
  add_right(add_left(s, left_pad), right_pad)
);

export let pad_left := (s, width, ch) => (
  let len := rt.str_len(s);
  (rt.str_repeat(ch, width - len) + s if len < width | s if _)
);

export let pad_right := (s, width, ch) => (
  let len := rt.str_len(s);
  (s + rt.str_repeat(ch, width - len) if len < width | s if _)
);
