//! String builder: accumulate string parts and join them in a single pass.

import "@std/rt" as rt;

/// Mutable string builder that collects parts for efficient concatenation.
export let Builder := record { parts : []String };

/// Create an empty string builder.
export let new := () => .{ parts := [] };

/// Append a string to the builder.
export let append := (b, s : String) => rt.arr_push(b.parts, s);

/// Append a string followed by a newline.
export let append_line := (b, s) => rt.arr_push(b.parts, s + "\n");

/// Concatenate all accumulated parts into a single string.
export let build := (b) => rt.str_join(b.parts, "");

/// Return the number of parts in the builder.
export let len := (b) => rt.arr_len(b.parts);
