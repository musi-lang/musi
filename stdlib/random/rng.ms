//! Xorshift PRNG - core RNG type and basic operations.

import "@std/rt" as rt;

/// Mutable RNG state holding the current seed.
export let Rng := record { seed : Int };

let MASK := 140737488355327;

/// Advances the RNG state and returns the raw next value.
export let next_raw := (mut rng) => (
  let s0 := rng.seed;
  let s1 := s0 xor rt.int_shl(s0, 13);
  let s2 := s1 xor rt.int_shr(s1, 7);
  let s3 := s2 xor rt.int_shl(s2, 17);
  let masked := (s3 + MASK + 1) - (((s3 + MASK + 1) / (MASK + 1)) * (MASK + 1));
  let positive := (masked if masked > 0 | 1 if _);
  rng.seed <- positive;
  positive
);

/// Creates a new RNG seeded with the given value (minimum seed is 1).
export let new := (seed) => (
  let s := (seed if seed > 0 | 1 if _);
  .{ seed := s }
);

/// Reseeds an existing RNG with a new seed value.
export let seed_from := (mut rng, new_seed) =>
  rng.seed <- (new_seed if new_seed > 0 | 1 if _);

/// Returns the next non-negative random integer.
export let next_int := (rng) => next_raw(rng);

/// Returns the next random integer in the inclusive range [lo, hi].
export let next_bounded := (rng, lo, hi) => (
  let range := hi - lo + 1;
  let raw := next_raw(rng);
  lo + (raw - ((raw / range) * range))
);

/// Returns the next random float in [0.0, 1.0).
export let next_float := (rng) => (
  let raw := next_raw(rng);
  rt.int_to_float(raw) / rt.int_to_float(MASK)
);

/// Returns a random boolean.
export let next_bool := (rng) => (
  let raw := next_raw(rng);
  (raw - ((raw / 2) * 2)) = 1
);
