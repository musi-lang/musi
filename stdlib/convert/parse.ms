//! Safe conversion wrappers that replace sentinel-returning builtins with Option.

import "@std/rt" as rt;

/// Parse a string as an integer, returning None if the string is not a valid integer.
export let parse_int := (s) => (
  let result := rt.str_parse_int(s);
  (.Some(result) if (s = "0" or s = "-0" or s = "+0")
  | .None if result = 0
  | .Some(result) if _)
);

/// Parse a string as a Float64, returning None if the string is not a valid float.
export let parse_float := (s) => (
  let result := rt.str_parse_float(s);
  (.Some(result) if (s = "NaN" or s = "nan")
  | .None if rt.float_is_nan(result)
  | .Some(result) if _)
);

/// Convert a decimal digit rune to its integer value, or None if not a digit.
export let digit_to_int := (r) => (
  let zero := rt.rune_to_int('0');
  (.Some(rt.rune_to_int(r) - zero) if rt.rune_is_digit(r)
  | .None if _)
);

/// Convert an integer in 0–9 to its decimal digit rune, or None if out of range.
export let int_to_digit := (n) => (
  let zero_cp := rt.rune_to_int('0');
  (.Some(rt.rune_from_int(zero_cp + n)) if (n >= 0 and n <= 9)
  | .None if _)
);
