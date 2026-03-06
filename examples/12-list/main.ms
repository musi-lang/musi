import { List, Nil, Cons, list_length, list_append, list_reverse,
         list_map, list_filter, list_fold, list_head, list_sum } from "std/core/List";

// Build a list: 1 :: 2 :: 3 :: 4 :: 5 :: Nil
const xs := Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))));

writeln(int_to_string(list_length(xs)));       // 5

// sum
writeln(int_to_string(list_sum(xs)));          // 15

// map (* 2)
const doubled := list_map(xs, fn(x: Int): Int => x * 2);
writeln(int_to_string(list_sum(doubled)));     // 30

// filter (> 2)
const big := list_filter(xs, fn(x: Int): Bool => x > 2);
writeln(int_to_string(list_length(big)));      // 3

// fold: product
const prod := list_fold(xs, 1, fn(acc: Int, x: Int): Int => acc * x);
// (fold uses (acc, x) -> acc op x form)
writeln(int_to_string(prod));                  // 120

// reverse
const rev := list_reverse(xs);
const rev_sum := list_fold(rev, 0, fn(acc: Int, x: Int): Int => acc + x);
writeln(int_to_string(rev_sum));               // 15 (same sum)

// head
const h := list_head(xs);
writeln(match h with (
    None    => "none"
  | Some(v) => int_to_string(v)
));  // 1

// append
const ys := Cons(6, Cons(7, Nil));
const zs := list_append(xs, ys);
writeln(int_to_string(list_length(zs)));       // 7
