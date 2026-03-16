//! Safe conversion wrappers that replace sentinel-returning builtins with Option.

import "@std/rt" as rt;

export let parse_int := (s) => (
  let result := rt.str_parse_int(s);
  (.Some(result) if (s = "0" or s = "-0" or s = "+0")
  | .None if result = 0
  | .Some(result) if _)
);

export let parse_float := (s) => (
  let result := rt.str_parse_float(s);
  (.Some(result) if (s = "NaN" or s = "nan")
  | .None if rt.float_is_nan(result)
  | .Some(result) if _)
);

export let digit_to_int := (r) => (
  let zero := rt.rune_to_int('0');
  (.Some(rt.rune_to_int(r) - zero) if rt.rune_is_digit(r)
  | .None if _)
);

export let int_to_digit := (n) => (
  let zero_cp := rt.rune_to_int('0');
  (.Some(rt.rune_from_int(zero_cp + n)) if (n >= 0 and n <= 9)
  | .None if _)
);
