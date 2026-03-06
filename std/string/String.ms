#[intrinsic("string_length")]   export extrin fn string_length(s: String): Int;
#[intrinsic("string_concat")]   export extrin fn string_concat(a: String, b: String): String;
#[intrinsic("string_slice")]    export extrin fn string_slice(s: String, start: Int, end: Int): String;
#[intrinsic("string_to_int")]   export extrin fn string_to_int(s: String): Option[Int];
#[intrinsic("string_contains")] export extrin fn string_contains(s: String, sub: String): Bool;

export fn string_is_empty(s: String): Bool => string_length(s) = 0;

fn string_repeat_aux(s: String, acc: String, n: Int): String =>
    if n = 0 then acc else string_repeat_aux(s, string_concat(acc, s), n - 1);

export fn string_repeat(s: String, n: Int): String => string_repeat_aux(s, "", n);
