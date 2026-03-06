
export fn int_abs(n: Int): Int => if n < 0 then 0 - n else n;
export fn int_min(a: Int, b: Int): Int => if a < b then a else b;
export fn int_max(a: Int, b: Int): Int => if a > b then a else b;
export fn int_clamp(n: Int, lo: Int, hi: Int): Int => int_min(int_max(n, lo), hi);

export fn abs(n: Int): Int => int_abs(n);
export fn min(a: Int, b: Int): Int => int_min(a, b);
export fn max(a: Int, b: Int): Int => int_max(a, b);
export fn clamp(n: Int, lo: Int, hi: Int): Int => int_clamp(n, lo, hi);
