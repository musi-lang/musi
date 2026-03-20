//! FNV-1a hash functions.
//!
//! All functions return non-negative Int values suitable as hash codes.

import "@std/rt" as rt;

let FNV_OFFSET := 2166136261;
let FNV_PRIME  := 16777619;

// FNV multiplication stays within 47-bit safe range via masking.
let MASK := 140737488355327;

let fnv_mix := (h, byte) => (
  let xored := h xor byte;
  let product := xored * FNV_PRIME;
  product and MASK
);

/// Returns the FNV-1a hash of an integer.
export let hash_int := (n) => (
  let abs_n := (n if n >= 0 | 0 - n if _);
  let b0 := abs_n and 0xFF;
  let b1 := (abs_n / 256) and 0xFF;
  let b2 := (abs_n / 65536) and 0xFF;
  let b3 := (abs_n / 16777216) and 0xFF;
  let h0 := fnv_mix(FNV_OFFSET, b0);
  let h1 := fnv_mix(h0, b1);
  let h2 := fnv_mix(h1, b2);
  fnv_mix(h2, b3)
);

/// Returns the FNV-1a hash of a string.
export let hash_str := (s) => (
  let chars := rt.str_chars(s);
  let len := rt.arr_len(chars);
  let step := (i, h) => (
    step(i + 1, fnv_mix(h, rt.rune_to_int(chars.[i]))) if i < len
    | h if _
  );
  step(0, FNV_OFFSET)
);

/// Returns the FNV-1a hash of a boolean.
export let hash_bool := (b) => (
  hash_int(1 if b | 0 if _)
);

/// Combines two hash codes into a single hash code.
export let combine := (h1, h2) => (
  let mixed := h1 xor (h2 + 2654435769 + (h1 * 64) + (h1 / 4));
  (mixed if mixed >= 0 | 0 - mixed if _)
);
