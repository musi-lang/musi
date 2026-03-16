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
let OCT_8 := 56;
let OCT_9 := 57;
let BIN_0 := 48;
let BIN_1 := 49;

export let is_alpha := (r) => rt.rune_is_alpha(r);
export let is_digit := (r) => rt.rune_is_digit(r);
export let is_whitespace := (r) => rt.rune_is_whitespace(r);
export let is_alphanumeric := (r) => rt.rune_is_alpha(r) or rt.rune_is_digit(r);
export let is_upper := (r) =>
  rt.rune_is_alpha(r) and rt.rune_to_upper(r) = r and not (rt.rune_to_lower(r) = r);
export let is_lower := (r) =>
  rt.rune_is_alpha(r) and rt.rune_to_lower(r) = r and not (rt.rune_to_upper(r) = r);

export let to_upper := (r) => rt.rune_to_upper(r);
export let to_lower := (r) => rt.rune_to_lower(r);
export let to_int := (r) => rt.rune_to_int(r);
export let from_int := (n) => rt.rune_from_int(n);

export let is_ascii := (r) => (
  let cp := rt.rune_to_int(r);
  cp >= ASCII_MIN and cp <= ASCII_MAX
);

export let is_hex_digit := (r) => (
  let cp := rt.rune_to_int(r);
  (cp >= HEX_0 and cp <= HEX_9)
    or (cp >= HEX_A and cp <= HEX_F)
    or (cp >= HEX_a and cp <= HEX_f)
);

export let is_bin_digit := (r) => (
  let cp := rt.rune_to_int(r);
  cp >= BIN_0 and cp <= BIN_1
);

export let is_oct_digit := (r) => (
  let cp := rt.rune_to_int(r);
  (cp >= OCT_0 and cp <= OCT_7)
    or (cp >= OCT_8 and cp <= OCT_9)
);
