--! Array collection helpers.

--- Build an empty array.
export let empty [T] () := [];
--- Build a one-value array.
export let single [T] (value : T) := [value];
--- Build a two-value array.
export let pair [T] (first : T, second : T) := [first, second];
--- Return a shallow copy of array values.
export let copy [T] (values : []T) := [...values];
--- Concatenate two arrays.
export let concat [T] (left : []T, right : []T) := [...left, ...right];
--- Append one value to an array.
export let append [T] (values : []T, value : T) := [...values, value];
--- Function for `prepend`.
let prependValue [T] (values : []T, value : T) := [value, ...values];
--- Compare integer arrays for equality.
export let equalsInt (left : []Int, right : []Int) := left = right;
--- Compare integer arrays for inequality.
export let notEqualsInt (left : []Int, right : []Int) := left /= right;
--- Returns true when array has no values.
export let isEmpty [T] (values : []T) := if values = [] then 0 = 0 else 0 = 1;
--- Returns true when array has values.
export let nonEmpty [T] (values : []T) := if values = [] then 0 = 1 else 0 = 0;

--- Prepend a leading value.
export let prepend [T] (value : T, values : []T) :=
  prependValue[T](values, value);
