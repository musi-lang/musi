// std/string.ms — pure String utilities built on musi:string intrinsics
// Intrinsic ops: import { string_length, string_slice, string_to_int, string_contains, nat_to_string } from "musi:string"

import { string_length, string_slice } from "musi:string";

export fn string_is_empty(s: String): Bool => string_length(s) = 0;

export fn string_starts_with(s: String, prefix: String): Bool => (
    const plen := string_length(prefix);
    if plen > string_length(s) then .False
    else string_contains(string_slice(s, 0, plen), prefix)
);

export fn string_ends_with(s: String, suffix: String): Bool => (
    const slen := string_length(s);
    const sflen := string_length(suffix);
    if sflen > slen then .False
    else string_contains(string_slice(s, slen - sflen, slen), suffix)
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

export fn string_join(parts: []String, sep: String): String => (
    const n := array_length(parts);
    if n = 0 then ""
    else (
        var result := array_get(parts, 0);
        var i := 1;
        while i < n loop (
            result <- string_concat(result, string_concat(sep, array_get(parts, i)));
            i <- i + 1
        );
        result
    )
);

export fn string_lines(s: String): []String => string_split(s, "\n");

export fn string_words(s: String): []String => string_split(string_trim(s), " ");

export fn string_replace(s: String, pat: String, to: String): String =>
    string_join(string_split(s, pat), to);

// Return the byte at index i as a one-character string.
// Safe for ASCII; may return a partial UTF-8 sequence for multi-byte chars.
export fn string_char_at(s: String, i: Int): String => string_slice(s, i, i + 1);

// Count non-overlapping occurrences of sub in s.
export fn string_count(s: String, sub: String): Int =>
    array_length(string_split(s, sub)) - 1;
