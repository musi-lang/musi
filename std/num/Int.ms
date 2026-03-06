// int_to_string is in the prelude

export fn int_abs(n: Int): Int => if n < 0 then 0 - n else n;
export fn int_min(a: Int, b: Int): Int => if a < b then a else b;
export fn int_max(a: Int, b: Int): Int => if a > b then a else b;
export fn int_clamp(n: Int, lo: Int, hi: Int): Int => int_min(int_max(n, lo), hi);

// Short-name aliases (use via UFCS: n.abs(), n.clamp(lo, hi))
export fn abs(n: Int): Int => int_abs(n);
export fn clamp(n: Int, lo: Int, hi: Int): Int => int_clamp(n, lo, hi);
