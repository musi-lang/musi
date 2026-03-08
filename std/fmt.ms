// std/fmt.ms — formatting utilities
// import { format_int, format_float, format_list, ... } from "std/fmt"

/// Format an integer with thousand separators.
export fn format_int_sep(n: Int, sep: String): String => (
    const s := int_to_string(if n < 0 then 0 - n else n);
    const len := string_length(s);
    var result := "";
    var i := 0;
    while i < len loop (
        if i > 0 and (len - i) % 3 = 0 then result <- string_concat(result, sep);
        result <- string_concat(result, string_slice(s, i, i + 1));
        i <- i + 1
    );
    if n < 0 then string_concat("-", result) else result
);

/// Format an integer with comma separators (1,234,567).
export fn format_int(n: Int): String => format_int_sep(n, ",");

/// Format a float to `decimals` decimal places.
export fn format_float(f: Float, decimals: Int): String => (
    const neg := f < 0.0;
    const abs_f := if neg then 0.0 - f else f;
    const int_part := float_to_int(abs_f);
    const frac := abs_f - int_to_float(int_part);
    const mult := pow_10(decimals);
    const frac_int := float_to_int(frac * int_to_float(mult) + 0.5);
    const frac_str := pad_left_zeros(frac_int, decimals);
    const result := string_concat(string_concat(int_to_string(int_part), "."), frac_str);
    if neg then string_concat("-", result) else result
);

fn pow_10(n: Int): Int => (
    var result := 1;
    var i := 0;
    while i < n loop (result <- result * 10; i <- i + 1);
    result
);

fn pad_left_zeros(n: Int, width: Int): String => (
    var s := int_to_string(n);
    while string_length(s) < width loop (s <- string_concat("0", s));
    s
);

/// Format a percentage (0.0-1.0 → "XX.X%").
export fn format_percent(ratio: Float, decimals: Int): String =>
    string_concat(format_float(ratio * 100.0, decimals), "%");

/// Format bytes as human-readable (KB, MB, GB).
export fn format_bytes(bytes: Int): String => (
    if bytes < 1024 then string_concat(int_to_string(bytes), " B")
    else if bytes < 1048576 then string_concat(format_float(int_to_float(bytes) / 1024.0, 1), " KB")
    else if bytes < 1073741824 then string_concat(format_float(int_to_float(bytes) / 1048576.0, 1), " MB")
    else string_concat(format_float(int_to_float(bytes) / 1073741824.0, 2), " GB")
);

/// Format a duration in milliseconds as human-readable.
export fn format_duration_ms(ms: Int): String => (
    if ms < 1000 then string_concat(int_to_string(ms), "ms")
    else if ms < 60000 then string_concat(format_float(int_to_float(ms) / 1000.0, 1), "s")
    else if ms < 3600000 then string_concat(format_float(int_to_float(ms) / 60000.0, 1), "m")
    else string_concat(format_float(int_to_float(ms) / 3600000.0, 1), "h")
);

/// Format a list of strings with separator and final conjunction.
export fn format_list(items: []String, sep: String, conj: String): String => (
    const n := array_length(items);
    if n = 0 then ""
    else if n = 1 then array_get(items, 0)
    else if n = 2 then string_concat(string_concat(array_get(items, 0), conj), array_get(items, 1))
    else (
        var result := "";
        var i := 0;
        while i < n - 1 loop (
            if i > 0 then result <- string_concat(result, sep);
            result <- string_concat(result, array_get(items, i));
            i <- i + 1
        );
        string_concat(string_concat(result, conj), array_get(items, n - 1))
    )
);

/// Format a list with commas and "and" (e.g., "a, b, and c").
export fn format_list_and(items: []String): String => format_list(items, ", ", ", and ");

/// Format a list with commas and "or" (e.g., "a, b, or c").
export fn format_list_or(items: []String): String => format_list(items, ", ", ", or ");

/// Pluralize a word based on count.
export fn pluralize(count: Int, singular: String, plural: String): String =>
    if count = 1 then singular else plural;

/// Format count with noun (e.g., "1 item", "5 items").
export fn format_count(count: Int, singular: String, plural: String): String =>
    string_concat(string_concat(int_to_string(count), " "), pluralize(count, singular, plural));

/// Convert string to title case (capitalize first letter of each word).
export fn title_case(s: String): String => (
    const ws := string_split(s, " ");
    var result := [];
    for w in ws loop (
        if string_length(w) > 0 then (
            const first := string_to_upper(string_slice(w, 0, 1));
            const rest := string_to_lower(string_slice(w, 1, string_length(w)));
            array_push(result, string_concat(first, rest))
        )
    );
    join_fmt(result, " ")
);

fn join_fmt(parts: []String, sep: String): String => (
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
