// std/collections/set.ms — HashSet backed by Map (sentinel value "1")
// NOTE: Set handles are typed as String in signatures below; same placeholder as map.ms.
// import { set_new, set_add, set_has, set_remove, set_from_array, set_union, set_intersect, set_diff } from "std/collections/set"

export fn set_new(): String => map_new();
export fn set_add(s: String, key: String): Unit => map_set(s, key, "1");
export fn set_has(s: String, key: String): Bool => map_has(s, key);
export fn set_remove(s: String, key: String): Unit => map_delete(s, key);
export fn set_to_array(s: String): []String => map_keys(s);
export fn set_len(s: String): Int => map_len(s);

export fn set_from_array(arr: []String): String => (
    const s := set_new();
    for x in arr loop (set_add(s, x));
    s
);

export fn set_union(a: String, b: String): String => (
    const r := set_new();
    for k in map_keys(a) loop (set_add(r, k));
    for k in map_keys(b) loop (set_add(r, k));
    r
);

export fn set_intersect(a: String, b: String): String => (
    const r := set_new();
    for k in map_keys(a) loop (if set_has(b, k) then set_add(r, k));
    r
);

export fn set_diff(a: String, b: String): String => (
    const r := set_new();
    for k in map_keys(a) loop (if not set_has(b, k) then set_add(r, k));
    r
);
