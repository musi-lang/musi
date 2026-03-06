#[intrinsic("float_to_string")] export native fn float_to_string(n: Float): String;

export fn float_abs(n: Float): Float => if n < 0.0 then 0.0 - n else n;
export fn float_min(a: Float, b: Float): Float => if a < b then a else b;
export fn float_max(a: Float, b: Float): Float => if a > b then a else b;
export fn float_clamp(n: Float, lo: Float, hi: Float): Float => float_min(float_max(n, lo), hi);

// Short-name alias (use via UFCS: n.fabs())
export fn fabs(n: Float): Float => float_abs(n);
