// std/collections/set.ms — HashSet backed by Map[String, String] (sentinel value "1")
// import { set_new, set_add, set_has, set_remove, set_to_array, set_len,
//          set_from_array, set_union, set_intersect, set_diff } from "std/collections/set"

export fn set_new(): Map[String, String] => map_new();
export fn set_add(s: Map[String, String], key: String): Unit => map_set(s, key, "1");
export fn set_has(s: Map[String, String], key: String): Bool => map_has(s, key);
export fn set_remove(s: Map[String, String], key: String): Unit => map_delete(s, key);
export fn set_to_array(s: Map[String, String]): []String => map_keys(s);
export fn set_len(s: Map[String, String]): Int => map_len(s);

export fn set_from_array(arr: []String): Map[String, String] => (
    const s := set_new();
    for x in arr loop (set_add(s, x));
    s
);

export fn set_union(a: Map[String, String], b: Map[String, String]): Map[String, String] => (
    const r := set_new();
    for k in map_keys(a) loop (set_add(r, k));
    for k in map_keys(b) loop (set_add(r, k));
    r
);

export fn set_intersect(a: Map[String, String], b: Map[String, String]): Map[String, String] => (
    const r := set_new();
    for k in map_keys(a) loop (if set_has(b, k) then set_add(r, k));
    r
);

export fn set_diff(a: Map[String, String], b: Map[String, String]): Map[String, String] => (
    const r := set_new();
    for k in map_keys(a) loop (if not set_has(b, k) then set_add(r, k));
    r
);
