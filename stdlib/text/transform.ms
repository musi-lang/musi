//! String transformation utilities.

import "@std/rt" as rt;

export let reverse_str := (s) => (
  let chars := rt.str_chars(s);
  rt.arr_reverse(chars);
  rt.str_from_chars(chars)
);

export let truncate := (s, max_len) => (
  rt.str_slice(s, 0, max_len) if rt.str_len(s) > max_len
  | s if _
);

export let remove_prefix := (s, pre) => (
  rt.str_slice(s, rt.str_len(pre), rt.str_len(s)) if rt.str_starts_with(s, pre)
  | s if _
);

export let remove_suffix := (s, suf) => (
  rt.str_slice(s, 0, rt.str_len(s) - rt.str_len(suf)) if rt.str_ends_with(s, suf)
  | s if _
);
