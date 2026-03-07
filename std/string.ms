// std/string.ms — String utilities
// import { string_length, string_slice, string_contains, string_is_empty, ... } from "std/string"

#[intrinsic("string_length")]   export extrin fn string_length(s: String): Int;
#[intrinsic("string_slice")]    export extrin fn slice(s: String, start: Int, end: Int): String;
#[intrinsic("string_to_int")]   export extrin fn parse_int(s: String): Option[Int];
#[intrinsic("string_contains")] export extrin fn string_contains(s: String, sub: String): Bool;

export fn string_is_empty(s: String): Bool => string_length(s) = 0;

export fn string_starts_with(s: String, prefix: String): Bool => (
    const plen := string_length(prefix);
    if plen > string_length(s) then .False
    else string_contains(slice(s, 0, plen), prefix)
);

export fn string_ends_with(s: String, suffix: String): Bool => (
    const slen := string_length(s);
    const sflen := string_length(suffix);
    if sflen > slen then .False
    else string_contains(slice(s, slen - sflen, slen), suffix)
);

fn repeat_aux(s: String, acc: String, n: Int): String =>
    if n = 0 then acc else repeat_aux(s, string_concat(acc, s), n - 1);

export fn string_repeat(s: String, n: Int): String => repeat_aux(s, "", n);

export fn string_pad_start(s: String, width: Int, pad: String): String => (
    var result := s;
    while string_length(result) < width loop (result <- string_concat(pad, result));
    result
);

export fn string_pad_end(s: String, width: Int, pad: String): String => (
    var result := s;
    while string_length(result) < width loop (result <- string_concat(result, pad));
    result
);
