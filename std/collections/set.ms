// std/collections/set.ms — HashSet backed by Map[String, String] (sentinel value "1")
// import { set_new, set_add, set_has, set_remove, set_to_array, set_len,
//          set_from_array, set_union, set_intersect, set_diff } from "std/collections/set"

/// Create an empty string set.
export fn set_new(): Map[String, String] => map_new();
/// Add `key` to the set (no-op if already present).
export fn set_add(s: Map[String, String], key: String): Unit => map_set(s, key, "1");
/// Return `.True` if `key` is in the set.
export fn set_has(s: Map[String, String], key: String): Bool => map_has(s, key);
/// Remove `key` from the set (no-op if absent).
export fn set_remove(s: Map[String, String], key: String): Unit => map_delete(s, key);
/// Return all set elements as an array (order is unspecified).
export fn set_to_array(s: Map[String, String]): []String => map_keys(s);
/// Return the number of elements in the set.
export fn set_len(s: Map[String, String]): Int => map_len(s);

/// Build a set from an array of strings.
export fn set_from_array(arr: []String): Map[String, String] => (
    const s := set_new();
    for x in arr loop (set_add(s, x));
    s
);

/// Return a new set containing all elements from either `a` or `b`.
export fn set_union(a: Map[String, String], b: Map[String, String]): Map[String, String] => (
    const r := set_new();
    for k in map_keys(a) loop (set_add(r, k));
    for k in map_keys(b) loop (set_add(r, k));
    r
);

/// Return a new set containing only elements present in both `a` and `b`.
export fn set_intersect(a: Map[String, String], b: Map[String, String]): Map[String, String] => (
    const r := set_new();
    for k in map_keys(a) loop (if set_has(b, k) then set_add(r, k));
    r
);

/// Return a new set containing elements in `a` that are not in `b`.
export fn set_diff(a: Map[String, String], b: Map[String, String]): Map[String, String] => (
    const r := set_new();
    for k in map_keys(a) loop (if not set_has(b, k) then set_add(r, k));
    r
);
