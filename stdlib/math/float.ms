//! Floating-point operations.

import "@std/rt" as rt;

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
