//! String case transformations.

import "@std/rt" as rt;

/// Return the string with its first character uppercased.
export let capitalize := (s) => (
  "" if rt.str_len(s) = 0
  | rt.str_to_upper(rt.str_slice(s, 0, 1)) + rt.str_slice(s, 1, rt.str_len(s)) if _
);

/// Return the string with its first character lowercased.
export let uncapitalize := (s) => (
  "" if rt.str_len(s) = 0
  | rt.str_to_lower(rt.str_slice(s, 0, 1)) + rt.str_slice(s, 1, rt.str_len(s)) if _
);
