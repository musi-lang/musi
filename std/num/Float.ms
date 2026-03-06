export fn float_abs(n: Float): Float => if n < 0.0 then 0.0 - n else n;
export fn float_min(a: Float, b: Float): Float => if a < b then a else b;
export fn float_max(a: Float, b: Float): Float => if a > b then a else b;
export fn float_clamp(n: Float, lo: Float, hi: Float): Float => float_min(float_max(n, lo), hi);

export fn fabs(n: Float): Float => float_abs(n);
export fn fmin(a: Float, b: Float): Float => float_min(a, b);
export fn fmax(a: Float, b: Float): Float => float_max(a, b);
export fn fclamp(n: Float, lo: Float, hi: Float): Float => float_clamp(n, lo, hi);
