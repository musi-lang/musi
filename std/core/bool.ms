// std/core/bool.ms — Bool utilities beyond the built-in operators (not, and, or, xor)
// import { bool_to_string, bool_from_int } from "std/core/bool"

/// Convert a `Bool` to the string `"true"` or `"false"`.
export fn bool_to_string(b: Bool): String => match b with (
    .False => "false"
  | .True  => "true"
);

/// Convert an integer to `Bool`: zero → `.False`, non-zero → `.True`.
export fn bool_from_int(n: Int): Bool => if n = 0 then .False else .True;
