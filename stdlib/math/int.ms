//! Integer operations.

import "@std/rt" as rt;

/// Returns the absolute value of an integer.
export let abs := (n) => rt.int_abs(n);

/// Returns the smaller of two integers.
export let min_int := (a, b) => rt.int_min(a, b);

/// Returns the larger of two integers.
export let max_int := (a, b) => rt.int_max(a, b);

/// Clamps an integer to the inclusive range [lo, hi].
export let clamp_int := (val, lo, hi) => rt.int_clamp(val, lo, hi);

/// Raises base to a non-negative integer exponent.
export let pow := (base, exp) => rt.int_pow(base, exp);

/// Returns -1, 0, or 1 depending on the sign of n.
export let sign_int := (n) => (
  -1 if n < 0
  | 0 if n = 0
  | 1 if _
);

/// Returns .True if n is divisible by 2.
export let is_even := (n) => n % 2 = 0;

/// Returns .True if n is not divisible by 2.
export let is_odd := (n) => n % 2 /= 0;

/// Returns the greatest common divisor of two integers.
export let gcd := (a, b) => (
  let step := (a, b) => (
    b if a = 0
    | step(b % a, a) if _
  );
  step(rt.int_abs(a), rt.int_abs(b))
);

/// Returns the least common multiple of two integers.
export let lcm := (a, b) => (
  0 if a = 0
  | 0 if b = 0
  | rt.int_abs(a * b) / gcd(a, b) if _
);
