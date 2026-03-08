// std/core/mod.ms — re-exports for core types
// import { Option, Expect, Ordering, Pair, Bool } from "std/core/mod"

export { Option, option_map, option_flat_map, option_unwrap_or,
         option_is_some, option_is_none } from "./option";

export { Expect, expect_map, expect_flat_map, expect_unwrap_or,
         expect_is_pass, expect_is_fail } from "./result";

export { Ordering, compare_int, compare_float, compare_string,
         ordering_reverse, ordering_then } from "./ordering";

export { Pair, pair_first, pair_second, pair_swap, pair_map } from "./pair";

export { Bool, bool_and, bool_or, bool_not, bool_xor,
         bool_to_string, bool_to_int } from "./bool";
