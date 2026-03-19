//! Floating-point operations.

import "@std/rt" as rt;

/// Returns the largest integer not greater than f.
export let floor := (f) => rt.float_floor(f);

/// Returns the smallest integer not less than f.
export let ceil := (f) => rt.float_ceil(f);

/// Rounds f to the nearest integer value.
export let round := (f) => rt.float_round(f);

/// Returns the square root of f.
export let sqrt := (f) => rt.float_sqrt(f);

/// Returns the sine of f (radians).
export let sin := (f) => rt.float_sin(f);

/// Returns the cosine of f (radians).
export let cos := (f) => rt.float_cos(f);

/// Returns the tangent of f (radians).
export let tan := (f) => rt.float_tan(f);

/// Returns the natural logarithm of f.
export let log := (f) => rt.float_log(f);

/// Returns e raised to the power f.
export let exp := (f) => rt.float_exp(f);

/// Returns .True if f is NaN.
export let is_nan := (f) => rt.float_is_nan(f);

/// Returns .True if f is positive or negative infinity.
export let is_infinite := (f) => rt.float_is_infinite(f);

/// Returns the absolute value of f.
export let abs_float := (f) => rt.float_abs(f);

/// Clamps f to the inclusive range [lo, hi].
export let clamp_float := (val, lo, hi) => (
  lo if val < lo
  | hi if val > hi
  | val if _
);

/// Returns the smaller of two floats.
export let min_float := (a, b) => (
  a if a < b
  | b if _
);

/// Returns the larger of two floats.
export let max_float := (a, b) => (
  a if a > b
  | b if _
);

/// Returns -1.0, 0.0, or 1.0 depending on the sign of f.
export let sign_float := (f) => (
  -1.0 if f < 0.0
  | 0.0 if f = 0.0
  | 1.0 if _
);

/// Linearly interpolates between a and b by t.
export let lerp := (a, b, t) => a + (b - a) * t;
