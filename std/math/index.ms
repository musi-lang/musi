/// The ratio of a circle's circumference to its diameter.
///
/// Approximately 3.14159265358979323846.
export let PI : Float64 := 3.14159265358979323846;

/// Euler's number, the base of natural logarithms.
///
/// Approximately 2.71828182845904523536.
export let E : Float64 := 2.71828182845904523536;

/// Compute the square root.
export let sqrt : Float64 -> Float64;

/// Compute the absolute value.
export let abs : Float64 -> Float64;

/// Round down to the nearest integer.
export let floor : Float64 -> Float64;

/// Round up to the nearest integer.
export let ceil : Float64 -> Float64;

/// Round to the nearest integer, half away from zero.
export let round : Float64 -> Float64;

/// Raise `base` to the power `exp`.
///
/// # Examples
///
/// ```musi
/// pow(2.0, 10.0)  // 1024.0
/// ```
export let pow : (Float64, Float64) -> Float64;

/// Compute the natural logarithm (base *e*).
export let log : Float64 -> Float64;

/// Compute the base-2 logarithm.
export let log2 : Float64 -> Float64;

/// Compute the base-10 logarithm.
export let log10 : Float64 -> Float64;

/// Compute the sine.
export let sin : Float64 -> Float64;

/// Compute the cosine.
export let cos : Float64 -> Float64;

/// Compute the tangent.
export let tan : Float64 -> Float64;

/// Compute the arcsine.
export let asin : Float64 -> Float64;

/// Compute the arccosine.
export let acos : Float64 -> Float64;

/// Compute the arctangent.
export let atan : Float64 -> Float64;

/// Compute the two-argument arctangent of `y` and `x`.
export let atan2 : (Float64, Float64) -> Float64;

/// Clamp a value to the range [lo, hi].
///
/// Returns `lo` if `x < lo`, `hi` if `x > hi`, otherwise `x`.
///
/// # Examples
///
/// ```musi
/// clamp(15.0, 0.0, 10.0)  // 10.0
/// ```
export let clamp : (Float64, Float64, Float64) -> Float64 :=
    (x, lo, hi) => (lo if x < lo | hi if x > hi | x if _);

/// Return the sign of a value as -1, 0, or 1.
export let sign : Float64 -> Float64 :=
    (x) => (-1.0 if x < 0.0 | 1.0 if x > 0.0 | 0.0 if _);
