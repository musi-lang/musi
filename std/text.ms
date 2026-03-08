// std/text.ms — text processing utilities
// import { words, lines, unwords, unlines, pad_left, pad_right, repeat, ... } from "std/text"

/// Split a string into words (by whitespace).
export fn words(s: String): []String => (
    const parts := string_split(string_trim(s), " ");
    var result := [];
    var i := 0;
    while i < array_length(parts) loop (
        if string_length(array_get(parts, i)) > 0 then
            array_push(result, array_get(parts, i));
        i <- i + 1
    );
    result
);

/// Split a string into lines (by newline).
export fn lines(s: String): []String => string_split(s, "\n");

/// Join words with a single space.
export fn unwords(ws: []String): String => join(ws, " ");

/// Join lines with newline characters.
export fn unlines(ls: []String): String => join(ls, "\n");

/// Join an array of strings with a separator.
export fn join(parts: []String, sep: String): String => (
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

/// Repeat a string `n` times.
export fn repeat(s: String, n: Int): String => (
    var result := "";
    var i := 0;
    while i < n loop (
        result <- string_concat(result, s);
        i <- i + 1
    );
    result
);

/// Pad a string on the left to reach `width`, using `pad` character.
export fn pad_left(s: String, width: Int, pad: String): String => (
    const len := string_length(s);
    if len >= width then s
    else string_concat(repeat(pad, width - len), s)
);

/// Pad a string on the right to reach `width`, using `pad` character.
export fn pad_right(s: String, width: Int, pad: String): String => (
    const len := string_length(s);
    if len >= width then s
    else string_concat(s, repeat(pad, width - len))
);

/// Center a string within `width`, padding both sides with `pad`.
export fn center(s: String, width: Int, pad: String): String => (
    const len := string_length(s);
    if len >= width then s
    else (
        const total_pad := width - len;
        const left_pad := total_pad / 2;
        const right_pad := total_pad - left_pad;
        string_concat(string_concat(repeat(pad, left_pad), s), repeat(pad, right_pad))
    )
);

/// Return `.True` if `s` starts with `prefix`.
export fn starts_with(s: String, prefix: String): Bool => (
    const plen := string_length(prefix);
    const slen := string_length(s);
    if plen > slen then .False
    else string_slice(s, 0, plen) = prefix
);

/// Return `.True` if `s` ends with `suffix`.
export fn ends_with(s: String, suffix: String): Bool => (
    const suflen := string_length(suffix);
    const slen := string_length(s);
    if suflen > slen then .False
    else string_slice(s, slen - suflen, slen) = suffix
);

/// Replace all occurrences of `pattern` with `replacement` in `s`.
export fn replace(s: String, pattern: String, replacement: String): String => (
    const parts := string_split(s, pattern);
    join(parts, replacement)
);

/// Remove `prefix` from the start of `s` if present.
export fn strip_prefix(s: String, prefix: String): String => (
    if starts_with(s, prefix)
    then string_slice(s, string_length(prefix), string_length(s))
    else s
);

/// Remove `suffix` from the end of `s` if present.
export fn strip_suffix(s: String, suffix: String): String => (
    if ends_with(s, suffix)
    then string_slice(s, 0, string_length(s) - string_length(suffix))
    else s
);

/// Return `.True` if the string is empty.
export fn is_empty(s: String): Bool => string_length(s) = 0;

/// Return `.True` if the string is empty or contains only whitespace.
export fn is_blank(s: String): Bool => string_length(string_trim(s)) = 0;

/// Reverse a string (byte-wise; not Unicode-aware).
export fn reverse(s: String): String => (
    const n := string_length(s);
    var result := "";
    var i := n - 1;
    while i >= 0 loop (
        result <- string_concat(result, string_slice(s, i, i + 1));
        i <- i - 1
    );
    result
);

/// Return the character (as a single-char string) at byte index `i`.
export fn char_at(s: String, i: Int): String => string_slice(s, i, i + 1);

/// Count occurrences of `sub` in `s`.
export fn count(s: String, sub: String): Int => (
    const parts := string_split(s, sub);
    array_length(parts) - 1
);

/// Truncate string to `max_len`, adding `ellipsis` if truncated.
export fn truncate(s: String, max_len: Int, ellipsis: String): String => (
    if string_length(s) <= max_len then s
    else (
        const elen := string_length(ellipsis);
        string_concat(string_slice(s, 0, max_len - elen), ellipsis)
    )
);
