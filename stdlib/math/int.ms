//! Integer operations.

import "@std/rt" as rt;

export let abs := (n) => rt.int_abs(n);

export let min_int := (a, b) => rt.int_min(a, b);

export let max_int := (a, b) => rt.int_max(a, b);

export let clamp_int := (val, lo, hi) => rt.int_clamp(val, lo, hi);

export let pow := (base, exp) => rt.int_pow(base, exp);

export let sign_int := (n) => (
  -1 if n < 0
  | 0 if n = 0
  | 1 if _
);

export let is_even := (n) => n % 2 = 0;

export let is_odd := (n) => n % 2 /= 0;

export let gcd := (a, b) => (
  let step := (a, b) => (
    b if a = 0
    | step(b % a, a) if _
  );
  step(rt.int_abs(a), rt.int_abs(b))
);

export let lcm := (a, b) => (
  0 if a = 0
  | 0 if b = 0
  | rt.int_abs(a * b) / gcd(a, b) if _
);
