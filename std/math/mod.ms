/// The ratio of a circle's circumference to its diameter.
export let PI : Float64 := 3.14159265358979323846;

/// Euler's number, the base of natural logarithms.
export let E : Float64 := 2.71828182845904523536;

/// Square root.
export let sqrt : Float64 -> Float64;

/// Absolute value.
export let abs : Float64 -> Float64;

/// Round down to the nearest integer.
export let floor : Float64 -> Float64;

/// Round up to the nearest integer.
export let ceil : Float64 -> Float64;

/// Round to the nearest integer, half away from zero.
export let round : Float64 -> Float64;

/// Raise `base` to the power `exp`.
export let pow : (Float64, Float64) -> Float64;

/// Natural logarithm (base `e`).
export let log : Float64 -> Float64;

/// Base-2 logarithm.
export let log2 : Float64 -> Float64;

/// Base-10 logarithm.
export let log10 : Float64 -> Float64;

/// Sine.
export let sin : Float64 -> Float64;

/// Cosine.
export let cos : Float64 -> Float64;

/// Tangent.
export let tan : Float64 -> Float64;

/// Arcsine.
export let asin : Float64 -> Float64;

/// Arccosine.
export let acos : Float64 -> Float64;

/// Arctangent.
export let atan : Float64 -> Float64;

/// Two-argument arctangent.
export let atan2 : (Float64, Float64) -> Float64;

/// Clamp a value to the range [lo, hi].
export let clamp : (Float64, Float64, Float64) -> Float64 :=
    (x, lo, hi) -> (lo if x < lo | hi if x > hi | x if _);

/// Return -1, 0, or 1 indicating the sign of the value.
export let sign : Float64 -> Float64 :=
    (x) -> (-1.0 if x < 0.0 | 1.0 if x > 0.0 | 0.0 if _);
