//! Runtime FFI bindings — the Musi runtime interface.
//! All functions are implemented in Rust and dispatched via the musi_rt virtual library.

#[link(name := "musi_rt")]
export foreign "C" (
  // String
  let str_len : (String) ~> Int;
  let str_byte_len : (String) ~> Int;
  let str_at : (String, Int) ~> Rune;
  let str_slice : (String, Int, Int) ~> String;
  let str_contains : (String, String) ~> Bool;
  let str_starts_with : (String, String) ~> Bool;
  let str_ends_with : (String, String) ~> Bool;
  let str_index_of : (String, String) ~> Int;
  let str_to_upper : (String) ~> String;
  let str_to_lower : (String) ~> String;
  let str_trim : (String) ~> String;
  let str_trim_start : (String) ~> String;
  let str_trim_end : (String) ~> String;
  let str_split : (String, String) ~> []String;
  let str_replace : (String, String, String) ~> String;
  let str_repeat : (String, Int) ~> String;
  let str_chars : (String) ~> []Rune;
  let str_from_chars : ([]Rune) ~> String;
  let str_parse_int : (String) ~> Int;
  let str_parse_float : (String) ~> Float64;
  let str_join : ([]String, String) ~> String;

  // Array
  let arr_len : ([]Any) ~> Int;
  let arr_push : ([]Any, Any) ~> Unit;
  let arr_pop : ([]Any) ~> Any;
  let arr_slice : ([]Any, Int, Int) ~> []Any;
  let arr_concat : ([]Any, []Any) ~> []Any;
  let arr_reverse : ([]Any) ~> Unit;
  let arr_contains : ([]Any, Any) ~> Bool;
  let arr_index_of : ([]Any, Any) ~> Int;
  let arr_sort : ([]Any) ~> Unit;

  // Int
  let int_abs : (Int) ~> Int;
  let int_min : (Int, Int) ~> Int;
  let int_max : (Int, Int) ~> Int;
  let int_clamp : (Int, Int, Int) ~> Int;
  let int_pow : (Int, Int) ~> Int;

  // Float
  let float_abs : (Float64) ~> Float64;
  let float_floor : (Float64) ~> Float64;
  let float_ceil : (Float64) ~> Float64;
  let float_round : (Float64) ~> Float64;
  let float_sqrt : (Float64) ~> Float64;
  let float_sin : (Float64) ~> Float64;
  let float_cos : (Float64) ~> Float64;
  let float_tan : (Float64) ~> Float64;
  let float_log : (Float64) ~> Float64;
  let float_exp : (Float64) ~> Float64;
  let float_is_nan : (Float64) ~> Bool;
  let float_is_infinite : (Float64) ~> Bool;

  // Float Constants
  let float_pi : () ~> Float64;
  let float_e : () ~> Float64;
  let float_infinity : () ~> Float64;
  let float_nan : () ~> Float64;

  // Int Bounds
  let int_min_val : () ~> Int;
  let int_max_val : () ~> Int;

  // Rune
  let rune_is_alpha : (Rune) ~> Bool;
  let rune_is_digit : (Rune) ~> Bool;
  let rune_is_whitespace : (Rune) ~> Bool;
  let rune_to_upper : (Rune) ~> Rune;
  let rune_to_lower : (Rune) ~> Rune;
  let rune_to_int : (Rune) ~> Int;
  let rune_from_int : (Int) ~> Rune;

  // Conversion
  let int_to_float : (Int) ~> Float64;
  let float_to_int : (Float64) ~> Int;
);

#[lang := "write" ]
#[link(name := "musi_rt")]
export foreign "C" (
  let write : (String) ~> Unit;
);

#[lang := "writeln" ]
#[link(name := "musi_rt")]
export foreign "C" (
  let writeln : (String) ~> Unit;
);

#[lang := "show" ]
#[link(name := "musi_rt")]
export foreign "C" (
  let show : (Any) ~> String;
);

#[lang := "str_cat" ]
#[link(name := "musi_rt")]
export foreign "C" (
  let str_cat : (String, String) ~> String;
);
