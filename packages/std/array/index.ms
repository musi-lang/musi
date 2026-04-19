let core := import "./_core.ms";

--- Build an empty sequence.
export let empty := core.empty;

--- Build a one-value array.
export let single := core.single;

--- Build a two-value array.
export let pair := core.pair;

--- Copy sequence values.
export let copy := core.copy;

--- Concatenate two sequences.
export let concat := core.concat;

--- Append a trailing value.
export let append := core.append;

--- Prepend a leading value.
export let prepend [T] (value : T, values : []T) : []T :=
    core.prepend[T](values, value);

--- Compare integer sequences for equality.
export let equalsInt := core.equalsInt;

--- Compare integer arrays for inequality.
export let notEqualsInt := core.notEqualsInt;

--- Returns true when sequence is empty.
export let isEmpty := core.isEmpty;

--- Returns true when sequence has values.
export let nonEmpty := core.nonEmpty;
