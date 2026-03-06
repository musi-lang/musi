import { unwrap_or, map, is_some } from "std/core/Option";
import { int_abs, int_clamp } from "std/num/Int";
import { string_length } from "std/string/String";
import { ordering_reverse } from "std/core/Ordering";
import { pair_new } from "std/core/Pair";

// Option
const opt := Some(21);
const doubled := map(opt, fn(n: Int): Int => n * 2);
writeln(int_to_string(unwrap_or(doubled, 0)));

// Int utils
writeln(int_to_string(int_abs(0 - 5)));
writeln(int_to_string(int_clamp(15, 0, 10)));

// String
writeln(int_to_string(string_length("hello")));

// Ordering
const ord := ordering_reverse(.Lt);
writeln(match ord with (
    .Lt => "lt"
  | .Eq => "eq"
  | .Gt => "gt"
));

// Pair
const p := pair_new(1, "one");
writeln(int_to_string(p.first));
