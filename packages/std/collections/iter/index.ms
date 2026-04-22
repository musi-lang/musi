let core := import "./core.ms";

--- Build an empty sequence.
export let empty := core.empty;

--- Build a one-value iterator.
export let once := core.once;

--- Concatenate two sequences.
export let concat := core.concat;

--- Append a trailing value.
export let append := core.append;

--- Prepend a leading value.
export let prepend := core.prepend;

--- Collect iterator values into an array.
export let collect := core.collect;

--- Compare integer sequences for equality.
export let equalsInt := core.equalsInt;

--- Returns true when sequence is empty.
export let isEmpty := core.isEmpty;

--- Returns true when sequence has values.
export let nonEmpty := core.nonEmpty;
