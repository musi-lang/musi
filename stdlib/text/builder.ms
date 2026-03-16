//! String builder: accumulate string parts and join them in a single pass.

import "../rt" as rt;

export let Builder := record { parts : [] String };

export let new := () => .{ parts := [] };

export let append := (b, s) => rt.arr_push(b.parts, s);

export let append_line := (b, s) => rt.arr_push(b.parts, s + "\n");

export let build := (b) => (
  let len := rt.arr_len(b.parts);
  let step := (acc, i) => (
    acc if i >= len
    | step(acc + b.parts.[i], i + 1) if _
  );
  step("", 0)
);

export let len := (b) => rt.arr_len(b.parts);
