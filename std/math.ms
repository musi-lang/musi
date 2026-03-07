// std/math.ms — integer and natural number math utilities
// Float math: import { sqrt, pow, floor, ceil, round, fabs, fmin, fmax, fclamp } from "musi:math"

// -- Int ----------------------------------------------------------------------

export fn abs(n: Int): Int => if n < 0 then 0 - n else n;
export fn min(a: Int, b: Int): Int => if a < b then a else b;
export fn max(a: Int, b: Int): Int => if a > b then a else b;
export fn clamp(n: Int, lo: Int, hi: Int): Int => min(max(n, lo), hi);
export fn sign(n: Int): Int => if n < 0 then 0 - 1 else if n > 0 then 1 else 0;

// -- Nat ----------------------------------------------------------------------

export fn nat_min(a: Nat, b: Nat): Nat => if a < b then a else b;
export fn nat_max(a: Nat, b: Nat): Nat => if a > b then a else b;
export fn nat_clamp(n: Nat, lo: Nat, hi: Nat): Nat => nat_min(nat_max(n, lo), hi);

// -- Integer algorithms -------------------------------------------------------

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
