//! Extended Unicode character (Rune) operations.

import "@std/rt" as rt;

export let is_alpha := (r) => rt.rune_is_alpha(r);

export let is_digit := (r) => rt.rune_is_digit(r);

export let is_whitespace := (r) => rt.rune_is_whitespace(r);

export let is_alphanumeric := (r) => rt.rune_is_alpha(r) or rt.rune_is_digit(r);

export let is_upper := (r) => rt.rune_is_alpha(r) and rt.rune_to_upper(r) = r and not (rt.rune_to_lower(r) = r);

export let is_lower := (r) => rt.rune_is_alpha(r) and rt.rune_to_lower(r) = r and not (rt.rune_to_upper(r) = r);

export let to_upper := (r) => rt.rune_to_upper(r);

export let to_lower := (r) => rt.rune_to_lower(r);

export let to_int := (r) => rt.rune_to_int(r);

export let from_int := (n) => rt.rune_from_int(n);

export let is_ascii := (r) => (
  let cp := rt.rune_to_int(r);
  cp >= 0 and cp <= 127
);

export let is_hex_digit := (r) => (
  let cp := rt.rune_to_int(r);
  (cp >= 48 and cp <= 57)
  or (cp >= 65 and cp <= 70)
  or (cp >= 97 and cp <= 102)
);
