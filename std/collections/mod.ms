// std/collections/mod.ms — re-exports for collections module
// import { array_map, range, List, Map, Set, Queue } from "std/collections/mod"

// Array utilities
export { array_map, array_filter, array_fold, array_each,
         array_any, array_all, array_find, array_contains,
         array_reverse, array_concat, array_flat_map, array_zip,
         array_sum, array_flatten, array_take, array_drop,
         array_sort_by, array_min_by, array_max_by, array_dedup,
         array_count_by } from "./array";

// Iterator combinators
export { range, range_step, enumerate, zip_arrays, repeat_val,
         cycle, take_while, drop_while, chunks, split_at,
         partition, interleave, last, first, init, tail,
         find_index, Indexed, Pair } from "./iter";

// Data structures
export { List, list_empty, list_cons, list_head, list_tail,
         list_is_empty, list_length, list_reverse, list_map,
         list_fold, list_to_array, array_to_list } from "./list";

export { Map, map_new, map_get, map_set, map_has,
         map_delete, map_keys, map_values, map_len } from "./map";

export { Set, set_new, set_add, set_has, set_delete,
         set_size, set_to_array } from "./set";

export { Queue, queue_new, queue_push, queue_pop,
         queue_peek, queue_is_empty, queue_len } from "./queue";
