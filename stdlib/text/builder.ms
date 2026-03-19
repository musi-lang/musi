//! String builder: accumulate string parts and join them in a single pass.

import "@std/rt" as rt;

export let Builder := record { parts : []String };

export let new := () => .{ parts := [] };

export let append := (b, s) => rt.arr_push(b.parts, s);

export let append_line := (b, s) => rt.arr_push(b.parts, s + "\n");

export let build := (b) => rt.str_join(b.parts, "");

export let len := (b) => rt.arr_len(b.parts);
