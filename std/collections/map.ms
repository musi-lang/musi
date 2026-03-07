// std/collections/map.ms — HashMap utilities
// NOTE: Map handles are typed as String in signatures below; this is a placeholder
// until a proper opaque Map type exists. The runtime value is Value::Map, not a string.
// Keys must be Int or String at runtime. Prelude exposes map_new/get/set/has/delete/keys/values/len.
// import { map_get_or, map_update, map_from_pairs, map_to_pairs, map_count_by, map_merge } from "std/collections/map"

import { unwrap_or } from "std/core/option";

export fn map_get_or(m: String, key: String, default: String): String =>
    unwrap_or(map_get(m, key), default);

export fn map_update(m: String, key: String, f: String -> String, default: String): Unit =>
    map_set(m, key, f(map_get_or(m, key, default)));

export fn map_from_pairs(pairs: [](String, String)): String => (
    const m := map_new();
    for pair in pairs loop (map_set(m, pair.0, pair.1));
    m
);

export fn map_to_pairs(m: String): [](String, String) => (
    var result := [];
    for k in map_keys(m) loop (
        array_push(result, (k, map_get_or(m, k, "")))
    );
    result
);

// Count occurrences; values stored as int strings ("0", "1", ...).
export fn map_count_by['T](arr: []'T, key_fn: 'T -> String): String => (
    const m := map_new();
    for x in arr loop (
        const k := key_fn(x);
        const cur_str := map_get_or(m, k, "0");
        const cur := unwrap_or(string_to_int(cur_str), 0);
        map_set(m, k, int_to_string(cur + 1))
    );
    m
);

export fn map_merge(a: String, b: String): String => (
    const r := map_new();
    for k in map_keys(a) loop (map_set(r, k, map_get_or(a, k, "")));
    for k in map_keys(b) loop (map_set(r, k, map_get_or(b, k, "")));
    r
);
