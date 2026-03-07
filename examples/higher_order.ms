// Higher-order functions, lambdas, and generics.

import { array_map, array_filter, array_fold } from "std/collections/array";

const numbers := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

// Filter to evens, double them, then sum.
const evens   := array_filter(numbers, fn (n) => n % 2 = 0);
const doubled := array_map(evens, fn (n) => n * 2);
const total   := array_fold(doubled, 0, fn (acc, n) => acc + n);

writeln(string_concat("sum of doubled evens: ", int_to_string(total)));

// Generic identity via lambda.
fn apply['T](f: 'T -> 'T, x: 'T): 'T => f(x);

writeln(int_to_string(apply(fn (n) => n * n, 7)));
writeln(apply(fn (s) => string_concat("<<", string_concat(s, ">>")), "hello"));
