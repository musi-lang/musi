// std/math.ms — numeric math utilities for Int, Float, and Nat
// import { abs, sqrt, pow, floor, ceil, round, min, max, clamp, PI } from "std/math"

// -- Float intrinsics --

#[intrinsic("float_sqrt")]  export extrin fn sqrt(n: Float): Float;
#[intrinsic("float_pow")]   export extrin fn pow(base: Float, exp: Float): Float;
#[intrinsic("float_floor")] export extrin fn floor(n: Float): Float;
#[intrinsic("float_ceil")]  export extrin fn ceil(n: Float): Float;

export fn round(n: Float): Float => floor(n + 0.5);

// -- Int --

export fn abs(n: Int): Int => if n < 0 then 0 - n else n;
export fn min(a: Int, b: Int): Int => if a < b then a else b;
export fn max(a: Int, b: Int): Int => if a > b then a else b;
export fn clamp(n: Int, lo: Int, hi: Int): Int => min(max(n, lo), hi);
export fn sign(n: Int): Int => if n < 0 then 0 - 1 else if n > 0 then 1 else 0;

// -- Float variants --

export fn fabs(n: Float): Float => if n < 0.0 then 0.0 - n else n;
export fn fmin(a: Float, b: Float): Float => if a < b then a else b;
export fn fmax(a: Float, b: Float): Float => if a > b then a else b;
export fn fclamp(n: Float, lo: Float, hi: Float): Float => fmin(fmax(n, lo), hi);

// -- Nat --

#[intrinsic("nat_to_string")] export extrin fn nat_to_string(n: Nat): String;

export fn nat_min(a: Nat, b: Nat): Nat => if a < b then a else b;
export fn nat_max(a: Nat, b: Nat): Nat => if a > b then a else b;
export fn nat_clamp(n: Nat, lo: Nat, hi: Nat): Nat => nat_min(nat_max(n, lo), hi);

// -- Integer math --

export fn gcd(a: Int, b: Int): Int =>
    if b = 0 then abs(a) else gcd(b, a % b);

export fn lcm(a: Int, b: Int): Int => (
    const g := gcd(a, b);
    if g = 0 then 0 else abs(a / g * b)
);

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
