//! UUID v4 generation.
//!
//! Generates random UUIDs using the caller-supplied Rng from stdlib/random.
//! Format: xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx

import "./rt" as rt;
import "./random" as random;

let HEX_CHARS := ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                   'a', 'b', 'c', 'd', 'e', 'f'];

let rand_nibble := (rng) => random.next_bounded(rng, 0, 15);

let nibble_char := (n) => HEX_CHARS.[n];

let rand_hex_char := (rng) => nibble_char(rand_nibble(rng));

let rand_hex_segment := (rng, count) => (
  let mut chars := [];
  let step := (i) => (
    (rt.arr_push(chars, rand_hex_char(rng)); step(i + 1)) if i < count
    | rt.str_from_chars(chars) if _
  );
  step(0)
);

export let v4 := (rng) => (
  let p1 := rand_hex_segment(rng, 8);
  let p2 := rand_hex_segment(rng, 4);
  // Version nibble is always '4'.
  let p3 := "4" + rand_hex_segment(rng, 3);
  // Variant nibble: must be 8, 9, a, or b (top two bits = 10).
  let variant_n := (rand_nibble(rng) and 3) + 8;
  let p4 := nibble_char(variant_n) + rand_hex_segment(rng, 3);
  let p5 := rand_hex_segment(rng, 12);
  p1 + "-" + p2 + "-" + p3 + "-" + p4 + "-" + p5
);
