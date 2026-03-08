// std/collections/map.ms — HashMap utilities
// import { map_get_or, map_update, map_from_pairs, map_to_pairs, map_count_by, map_merge } from "std/collections/map"

import { unwrap_or } from "std/core/option";

/// Look up `key` in the map, returning `default` if it is absent.
export fn map_get_or['K, 'V](m: Map['K, 'V], key: 'K, default: 'V): 'V =>
    unwrap_or(map_get(m, key), default);

/// Apply `f` to the current value at `key` (or `default` if absent) and store the result.
export fn map_update['K, 'V](m: Map['K, 'V], key: 'K, f: 'V -> 'V, default: 'V): Unit =>
    map_set(m, key, f(map_get_or(m, key, default)));

/// Build a map from an array of `(key, value)` pairs.
export fn map_from_pairs['K, 'V](pairs: []('K, 'V)): Map['K, 'V] => (
    const m := map_new();
    for pair in pairs loop (map_set(m, pair.0, pair.1));
    m
);

/// Convert a map to an array of `(key, value)` pairs (order is unspecified).
export fn map_to_pairs['K, 'V](m: Map['K, 'V]): []('K, 'V) => (
    var result := [];
    for k in map_keys(m) loop (
        match map_get(m, k) with (
            .Some(v) => array_push(result, (k, v))
            | .None => ()
        )
    );
    result
);

/// Count occurrences of each key produced by `key_fn` over an array.
export fn map_count_by['T](arr: []'T, key_fn: 'T -> String): Map[String, Int] => (
    const m := map_new();
    for x in arr loop (
        const k := key_fn(x);
        const cur := unwrap_or(map_get(m, k), 0);
        map_set(m, k, cur + 1)
    );
    m
);

/// Merge two maps; keys from `b` overwrite keys from `a`.
export fn map_merge['K, 'V](a: Map['K, 'V], b: Map['K, 'V]): Map['K, 'V] => (
    const r := map_new();
    for k in map_keys(a) loop (
        match map_get(a, k) with (.Some(v) => map_set(r, k, v) | .None => ())
    );
    for k in map_keys(b) loop (
        match map_get(b, k) with (.Some(v) => map_set(r, k, v) | .None => ())
    );
    r
);
