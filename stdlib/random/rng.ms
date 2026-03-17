//! Xorshift PRNG - core RNG type and basic operations.

import "@std/rt" as rt;

export let Rng := record { seed : Int };

let MASK := 140737488355327;

export let next_raw := (rng) => (
  let s0 := rng.seed;
  let s1 := s0 xor (s0 << 13);
  let s2 := s1 xor (s1 >> 7);
  let s3 := s2 xor (s2 << 17);
  let masked := (s3 + MASK + 1) - (((s3 + MASK + 1) / (MASK + 1)) * (MASK + 1));
  let positive := (masked if masked > 0 | 1 if _);
  rng.seed <- positive;
  positive
);

export let new := (seed) => (
  let s := (seed if seed > 0 | 1 if _);
  .{ seed := s }
);

export let seed_from := (rng, new_seed) =>
  rng.seed <- (new_seed if new_seed > 0 | 1 if _);

export let next_int := (rng) => next_raw(rng);

export let next_bounded := (rng, lo, hi) => (
  let range := hi - lo + 1;
  let raw := next_raw(rng);
  lo + (raw - ((raw / range) * range))
);

export let next_float := (rng) => (
  let raw := next_raw(rng);
  rt.int_to_float(raw) / rt.int_to_float(MASK)
);

export let next_bool := (rng) => (
  let raw := next_raw(rng);
  (raw - ((raw / 2) * 2)) = 1
);
