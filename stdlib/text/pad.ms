//! String padding utilities.

import "@std/rt" as rt;

export let pad_left := (s, width, ch) => (
  let len := rt.str_len(s);
  (rt.str_repeat(ch, width - len) + s if len < width | s if _)
);

export let pad_right := (s, width, ch) => (
  let len := rt.str_len(s);
  (s + rt.str_repeat(ch, width - len) if len < width | s if _)
);
