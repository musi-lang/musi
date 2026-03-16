//! String truncation with suffix.

import "@std/rt" as rt;

export let truncate := (s, max_len, suffix) => (
  let len := rt.str_len(s);
  let suf_len := rt.str_len(suffix);
  (rt.str_slice(s, 0, max_len - suf_len) + suffix if len > max_len | s if _)
);
