#[intrinsic("string_length")] export native fn string_length(s: String): Int;

export fn string_is_empty(s: String): Bool => string_length(s) = 0;
