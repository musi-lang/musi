import { musi_return_42, musi_add, musi_print_hello } from "./ffi";

writeln("Testing FFI...");

write("return_42 = ");
const x := musi_return_42();
writeln(int_to_string(x));

write("add(10,32) = ");
const sum := musi_add(10, 32);
writeln(int_to_string(sum));

write("print_hello: ");
musi_print_hello();

writeln("Done!");
