//! Xorshift PRNG — 47-bit positive range, safe for 48-bit signed ints.

import "./rt" as rt;

export let Rng := record { seed : Int };

let MASK := 140737488355327;  // (1 << 47) - 1

let next_raw := (rng) => (
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

export let choose ['a] := (rng, arr) => (
  let len := rt.arr_len(arr);
  (.Some(arr.[next_bounded(rng, 0, len - 1)]) if len > 0
  | .None if _)
);

export let shuffle ['a] := (rng, arr) => (
  let len := rt.arr_len(arr);
  let mut result := rt.arr_slice(arr, 0, len);
  let step := (i) => (
    (
      let j := next_bounded(rng, i, len - 1);
      let tmp := result.[i];
      result.[i] <- result.[j];
      result.[j] <- tmp;
      step(i + 1)
    ) if i < len - 1
    | result if _
  );
  (step(0) if len > 1 | result if _)
);

export let sample ['a] := (rng, arr, n) => (
  let len := rt.arr_len(arr);
  let count := (n if n < len | len if _);
  let shuffled := shuffle(rng, arr);
  rt.arr_slice(shuffled, 0, count)
);

export let next_gaussian := (rng) => (
  let u1 := next_float(rng);
  let u2 := next_float(rng);
  let safe_u1 := (u1 if u1 > 0.0 | 0.0001 if _);
  rt.float_sqrt(-2.0 * rt.float_log(safe_u1)) * rt.float_cos(2.0 * rt.float_pi() * u2)
);
