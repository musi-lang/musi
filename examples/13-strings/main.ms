import { string_concat, string_slice, string_to_int, string_contains, string_is_empty,
         string_repeat } from "std/string/String";

// string_concat
const hello := string_concat("Hello", ", world!");
writeln(hello);                                           // Hello, world!

// string_length (from prelude via String module)
writeln(int_to_string(string_length("hello")));           // 5 (but string_length is in prelude via String.ms)
// Actually string_length is importable from String too:

// string_slice
const sub := string_slice("abcdefgh", 2, 5);
writeln(sub);                                             // cde

// string_contains
const has := string_contains("hello world", "world");
writeln(match has with (
    False => "no"
  | True  => "yes"
));                                                       // yes

// string_to_int
const parsed := string_to_int("42");
writeln(match parsed with (
    None    => "failed"
  | Some(n) => int_to_string(n)
));                                                       // 42

const bad := string_to_int("abc");
writeln(match bad with (
    None    => "failed"
  | Some(_) => "ok"
));                                                       // failed

// string_is_empty
writeln(match string_is_empty("") with (
    False => "not empty"
  | True  => "empty"
));                                                       // empty

// string_repeat
writeln(string_repeat("ab", 3));                          // ababab
