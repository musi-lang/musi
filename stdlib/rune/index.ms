//! Extended Unicode character (Rune) operations.

import "@std/rt" as rt;

let ASCII_MIN := 0;
let ASCII_MAX := 127;
let HEX_0 := 48;
let HEX_9 := 57;
let HEX_A := 65;
let HEX_F := 70;
let HEX_a := 97;
let HEX_f := 102;
let OCT_0 := 48;
let OCT_7 := 55;
let BIN_0 := 48;
let BIN_1 := 49;

/// Returns .True if r is an alphabetic character.
export let is_alpha := (r) => rt.rune_is_alpha(r);

/// Returns .True if r is a decimal digit character.
export let is_digit := (r) => rt.rune_is_digit(r);

/// Returns .True if r is a whitespace character.
export let is_whitespace := (r) => rt.rune_is_whitespace(r);

/// Returns .True if r is alphabetic or a decimal digit.
export let is_alphanumeric := (r) => rt.rune_is_alpha(r) or rt.rune_is_digit(r);

/// Returns .True if r is an uppercase alphabetic character.
export let is_upper := (r) =>
  rt.rune_is_alpha(r) and rt.rune_to_upper(r) = r and not (rt.rune_to_lower(r) = r);

/// Returns .True if r is a lowercase alphabetic character.
export let is_lower := (r) =>
  rt.rune_is_alpha(r) and rt.rune_to_lower(r) = r and not (rt.rune_to_upper(r) = r);

/// Converts r to its uppercase form.
export let to_upper := (r) => rt.rune_to_upper(r);

/// Converts r to its lowercase form.
export let to_lower := (r) => rt.rune_to_lower(r);

/// Returns the Unicode codepoint of r as an integer.
export let to_int := (r) => rt.rune_to_int(r);

/// Creates a rune from a Unicode codepoint integer.
export let from_int := (n) => rt.rune_from_int(n);

/// Returns .True if r is in the ASCII range (0–127).
export let is_ascii := (r) => (
  let cp := rt.rune_to_int(r);
  cp >= ASCII_MIN and cp <= ASCII_MAX
);

/// Returns .True if r is a valid hexadecimal digit (0–9, a–f, A–F).
export let is_hex_digit := (r) => (
  let cp := rt.rune_to_int(r);
  (cp >= HEX_0 and cp <= HEX_9)
    or (cp >= HEX_A and cp <= HEX_F)
    or (cp >= HEX_a and cp <= HEX_f)
);

/// Returns .True if r is a binary digit (0 or 1).
export let is_bin_digit := (r) => (
  let cp := rt.rune_to_int(r);
  cp >= BIN_0 and cp <= BIN_1
);

/// Returns .True if r is an octal digit (0–7).
export let is_oct_digit := (r) => (
  let cp := rt.rune_to_int(r);
  cp >= OCT_0 and cp <= OCT_7
);
