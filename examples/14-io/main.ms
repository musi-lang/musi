import { read_line } from "std/io/Io";
import { string_concat } from "std/string/String";
import { float_sqrt, float_pow, float_floor, float_ceil } from "std/num/Float";

// Float math intrinsics
writeln(float_to_string(float_sqrt(2.0)));          // 1.4142135623730951
writeln(float_to_string(float_pow(2.0, 10.0)));     // 1024
writeln(float_to_string(float_floor(3.7)));         // 3
writeln(float_to_string(float_ceil(3.2)));          // 4

// Interactive: read a name and greet
write("Enter your name: ");
const name := read_line();
writeln(string_concat("Hello, ", string_concat(name, "!")));
