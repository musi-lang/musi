//! Numeric utilities: constants, integer operations, float operations.

import "./rt" as rt;

export let PI       := rt.float_pi();
export let E        := rt.float_e();
export let INFINITY := rt.float_infinity();
export let NAN      := rt.float_nan();
export let INT_MIN  := rt.int_min_val();
export let INT_MAX  := rt.int_max_val();

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

export let floor := (f) => rt.float_floor(f);

export let ceil := (f) => rt.float_ceil(f);

export let round := (f) => rt.float_round(f);

export let sqrt := (f) => rt.float_sqrt(f);

export let sin := (f) => rt.float_sin(f);

export let cos := (f) => rt.float_cos(f);

export let tan := (f) => rt.float_tan(f);

export let log := (f) => rt.float_log(f);

export let exp := (f) => rt.float_exp(f);

export let is_nan := (f) => rt.float_is_nan(f);

export let is_infinite := (f) => rt.float_is_infinite(f);

export let abs_float := (f) => rt.float_abs(f);

export let clamp_float := (val, lo, hi) => (
  lo if val < lo
  | hi if val > hi
  | val if _
);

export let min_float := (a, b) => (
  a if a < b
  | b if _
);

export let max_float := (a, b) => (
  a if a > b
  | b if _
);

export let sign_float := (f) => (
  -1.0 if f < 0.0
  | 0.0 if f = 0.0
  | 1.0 if _
);

export let lerp := (a, b, t) => a + (b - a) * t;
