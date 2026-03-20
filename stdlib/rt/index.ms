//! Runtime FFI bindings - the Musi runtime interface.
//! All functions are implemented in Rust and dispatched via the musi_rt virtual library.

#[link(name := "musi_rt")]
export foreign "C" (
  // String
  /// Return the character (rune) length of a string.
  let str_len : (String) ~> Int;
  /// Return the byte length of a string.
  let str_byte_len : (String) ~> Int;
  /// Return the rune at a given character index.
  let str_at : (String, Int) ~> Rune;
  /// Return the substring between two character indices.
  let str_slice : (String, Int, Int) ~> String;
  /// Return whether a string contains a substring.
  let str_contains : (String, String) ~> Bool;
  /// Return whether a string starts with a given prefix.
  let str_starts_with : (String, String) ~> Bool;
  /// Return whether a string ends with a given suffix.
  let str_ends_with : (String, String) ~> Bool;
  /// Return the first character index of a substring, or -1 if not found.
  let str_index_of : (String, String) ~> Int;
  /// Return a copy of the string with all characters uppercased.
  let str_to_upper : (String) ~> String;
  /// Return a copy of the string with all characters lowercased.
  let str_to_lower : (String) ~> String;
  /// Return a copy of the string with leading and trailing whitespace removed.
  let str_trim : (String) ~> String;
  /// Return a copy of the string with leading whitespace removed.
  let str_trim_start : (String) ~> String;
  /// Return a copy of the string with trailing whitespace removed.
  let str_trim_end : (String) ~> String;
  /// Split the string by a delimiter and return the parts as an array.
  let str_split : (String, String) ~> []String;
  /// Return a copy of the string with all occurrences of a pattern replaced.
  let str_replace : (String, String, String) ~> String;
  /// Return the string repeated n times.
  let str_repeat : (String, Int) ~> String;
  /// Return the string as an array of runes.
  let str_chars : (String) ~> []Rune;
  /// Construct a string from an array of runes.
  let str_from_chars : ([]Rune) ~> String;
  /// Parse a string as an integer, returning 0 on failure.
  let str_parse_int : (String) ~> Int;
  /// Parse a string as a Float64, returning NaN on failure.
  let str_parse_float : (String) ~> Float64;
  /// Join an array of strings with a separator.
  let str_join : ([]String, String) ~> String;

  // Array
  /// Return the number of elements in an array.
  let arr_len['T] : ([]'T) ~> Int;
  /// Append an element to the end of an array in place.
  let arr_push['T] : ([]'T, 'T) ~> Unit;
  /// Remove and return the last element of an array.
  let arr_pop['T] : ([]'T) ~> 'T;
  /// Return a sub-array between two indices.
  let arr_slice['T] : ([]'T, Int, Int) ~> []'T;
  /// Return a new array with two arrays concatenated.
  let arr_concat['T] : ([]'T, []'T) ~> []'T;
  /// Reverse an array in place.
  let arr_reverse['T] : ([]'T) ~> Unit;
  /// Return whether an array contains a given element.
  let arr_contains['T] : ([]'T, 'T) ~> Bool;
  /// Return the first index of an element in an array, or -1 if not found.
  let arr_index_of['T] : ([]'T, 'T) ~> Int;
  /// Sort an array in place using the natural ordering.
  let arr_sort['T] : ([]'T) ~> Unit;

  // Int
  /// Return the absolute value of an integer.
  let int_abs : (Int) ~> Int;
  /// Return the smaller of two integers.
  let int_min : (Int, Int) ~> Int;
  /// Return the larger of two integers.
  let int_max : (Int, Int) ~> Int;
  /// Clamp an integer to the inclusive range [lo, hi].
  let int_clamp : (Int, Int, Int) ~> Int;
  /// Return the base raised to the given integer exponent.
  let int_pow : (Int, Int) ~> Int;
  /// Shift an integer left by n bits.
  let int_shl : (Int, Int) ~> Int;
  /// Shift an integer right by n bits (arithmetic).
  let int_shr : (Int, Int) ~> Int;

  // Float
  /// Return the absolute value of a Float64.
  let float_abs : (Float64) ~> Float64;
  /// Return the largest integer value not greater than x.
  let float_floor : (Float64) ~> Float64;
  /// Return the smallest integer value not less than x.
  let float_ceil : (Float64) ~> Float64;
  /// Return x rounded to the nearest integer.
  let float_round : (Float64) ~> Float64;
  /// Return the square root of x.
  let float_sqrt : (Float64) ~> Float64;
  /// Return the sine of x (radians).
  let float_sin : (Float64) ~> Float64;
  /// Return the cosine of x (radians).
  let float_cos : (Float64) ~> Float64;
  /// Return the tangent of x (radians).
  let float_tan : (Float64) ~> Float64;
  /// Return the natural logarithm of x.
  let float_log : (Float64) ~> Float64;
  /// Return e raised to the power x.
  let float_exp : (Float64) ~> Float64;
  /// Return whether x is NaN.
  let float_is_nan : (Float64) ~> Bool;
  /// Return whether x is positive or negative infinity.
  let float_is_infinite : (Float64) ~> Bool;

  // Float Constants
  /// Return the mathematical constant pi.
  let float_pi : () ~> Float64;
  /// Return Euler's number e.
  let float_e : () ~> Float64;
  /// Return positive infinity.
  let float_infinity : () ~> Float64;
  /// Return the IEEE 754 NaN value.
  let float_nan : () ~> Float64;

  // Int Bounds
  /// Return the minimum representable integer value.
  let int_min_val : () ~> Int;
  /// Return the maximum representable integer value.
  let int_max_val : () ~> Int;

  // Rune
  /// Return whether a rune is an alphabetic character.
  let rune_is_alpha : (Rune) ~> Bool;
  /// Return whether a rune is a decimal digit.
  let rune_is_digit : (Rune) ~> Bool;
  /// Return whether a rune is a whitespace character.
  let rune_is_whitespace : (Rune) ~> Bool;
  /// Return the uppercase form of a rune.
  let rune_to_upper : (Rune) ~> Rune;
  /// Return the lowercase form of a rune.
  let rune_to_lower : (Rune) ~> Rune;
  /// Return the Unicode code point of a rune as an integer.
  let rune_to_int : (Rune) ~> Int;
  /// Return the rune corresponding to a Unicode code point.
  let rune_from_int : (Int) ~> Rune;

  // Conversion
  /// Convert an integer to a Float64.
  let int_to_float : (Int) ~> Float64;
  /// Truncate a Float64 to an integer.
  let float_to_int : (Float64) ~> Int;

  // Testing
  /// Return the test filter string set by the test runner.
  let __test_filter : () ~> String;
);

/// Write a string to standard output without a trailing newline.
#[lang := "write" ]
#[link(name := "musi_rt")]
export foreign "C" (
  let write : (String) ~> Unit;
);

/// Write a string to standard output followed by a newline.
#[lang := "writeln" ]
#[link(name := "musi_rt")]
export foreign "C" (
  let writeln : (String) ~> Unit;
);

/// Return the debug string representation of any value.
#[lang := "show" ]
#[link(name := "musi_rt")]
export foreign "C" (
  let show['T] : ('T) ~> String;
);

/// Concatenate two strings.
#[lang := "str_cat" ]
#[link(name := "musi_rt")]
export foreign "C" (
  let str_cat : (String, String) ~> String;
);
