import { option_unwrap_or } from "std/core/Option";

const v := option_unwrap_or(Some(42), 0);
writeln(int_to_string(v));
