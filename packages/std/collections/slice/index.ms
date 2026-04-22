let core := import "./core.ms";

--- Build an empty sequence.
export let empty := core.empty;

--- Copy sequence values.
export let copy := core.copy;

--- Concatenate two sequences.
export let concat := core.concat;

--- Compare integer sequences for equality.
export let equalsInt := core.equalsInt;

--- Returns true when sequence is empty.
export let isEmpty := core.isEmpty;

--- Returns true when sequence has values.
export let nonEmpty := core.nonEmpty;
