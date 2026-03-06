// Importing functions from the standard library
import { unwrap_or, map } from "std/core/Option";

const x := map(Some(21), fn(n: Int): Int => n * 2);
const y := unwrap_or(x, 0);
writeln(int_to_string(y));
