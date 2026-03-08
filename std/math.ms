// std/math.ms — integer and natural number math utilities
// Float math: import { sqrt, pow, floor, ceil, round, fabs, fmin, fmax, fclamp } from "musi:math"

// -- Int ----------------------------------------------------------------------

/// Absolute value of an integer.
export fn abs(n: Int): Int => if n < 0 then 0 - n else n;
/// Smaller of two integers.
export fn min(a: Int, b: Int): Int => if a < b then a else b;
/// Larger of two integers.
export fn max(a: Int, b: Int): Int => if a > b then a else b;
/// Clamp `n` to the range `[lo, hi]`.
export fn clamp(n: Int, lo: Int, hi: Int): Int => min(max(n, lo), hi);
/// Sign of an integer: `-1`, `0`, or `1`.
export fn sign(n: Int): Int => if n < 0 then 0 - 1 else if n > 0 then 1 else 0;

// -- Nat ----------------------------------------------------------------------

/// Smaller of two natural numbers.
export fn nat_min(a: Nat, b: Nat): Nat => if a < b then a else b;
/// Larger of two natural numbers.
export fn nat_max(a: Nat, b: Nat): Nat => if a > b then a else b;
/// Clamp a natural number to the range `[lo, hi]`.
export fn nat_clamp(n: Nat, lo: Nat, hi: Nat): Nat => nat_min(nat_max(n, lo), hi);

// -- Integer algorithms -------------------------------------------------------

/// Greatest common divisor of `a` and `b` (always non-negative).
export fn gcd(a: Int, b: Int): Int =>
    if b = 0 then abs(a) else gcd(b, a % b);

/// Least common multiple of `a` and `b` (always non-negative); `0` if either is `0`.
export fn lcm(a: Int, b: Int): Int => (
    const g := gcd(a, b);
    if g = 0 then 0 else abs(a / g * b)
);

// -- Float trig & transcendental (wraps musi:math / prelude intrinsics) ------

/// π ≈ 3.141592653589793
export fn pi(): Float => 3.141592653589793;
/// τ = 2π ≈ 6.283185307179586
export fn tau(): Float => 6.283185307179586;
/// Euler's number e ≈ 2.718281828459045
export fn euler_num(): Float => 2.718281828459045;
/// Convert radians to degrees.
export fn degrees(rad: Float): Float => rad * 180.0 / pi();
/// Convert degrees to radians.
export fn radians(deg: Float): Float => deg * pi() / 180.0;
/// Natural logarithm (base e).
export fn ln(x: Float): Float => log(x);
/// Base-2 logarithm.
export fn log2(x: Float): Float => log(x) / log(2.0);
/// Base-10 logarithm.
export fn log10(x: Float): Float => log(x) / log(10.0);
/// Absolute value of a float.
export fn float_abs(x: Float): Float => if x < 0.0 then 0.0 - x else x;
/// Sign of a float: `-1.0`, `0.0`, or `1.0`.
export fn float_sign(x: Float): Float => if x > 0.0 then 1.0 else if x < 0.0 then 0.0 - 1.0 else 0.0;

// -- Integer algorithms -------------------------------------------------------

/// Integer exponentiation: `base` raised to the power `exp` (non-negative exponent).
export fn pow_int(base: Int, exp: Int): Int => (
    var result := 1;
    var e := exp;
    var b := base;
    while e > 0 loop (
        if e % 2 = 1 then result <- result * b;
        b <- b * b;
        e <- e / 2
    );
    result
);
