// std/bool.ms — Bool utilities beyond the built-in operators (not, and, or, xor)
// import { bool_to_string, bool_from_int } from "std/bool"

// Bool → "true" / "false"
export fn bool_to_string(b: Bool): String => match b with (
    .False => "false"
  | .True  => "true"
);

// Non-zero → True, zero → False.
export fn bool_from_int(n: Int): Bool => if n = 0 then .False else .True;
