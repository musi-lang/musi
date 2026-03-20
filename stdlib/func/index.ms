//! Function combinators for Musi.

/// Return the argument unchanged.
export let identity ['a] := (x : 'a) => x;

/// Return a function that always returns `x`, ignoring its argument.
export let constant ['a, 'b] := (x : 'a) => (_ : 'b) => x;

/// Return a function that applies `f` after `g`.
export let compose ['a, 'b, 'c] := (f, g) => (x : 'a) => f(g(x));

/// Swap the two arguments of a binary function.
export let flip ['a, 'b, 'c] := (f) => (a : 'a, b : 'b) => f(b, a);

/// Apply `f` to `x`.
export let apply ['a, 'b] := (f, x : 'a) => f(x);

/// Lift a binary function to operate on the results of applying `g` to each argument.
export let on ['a, 'b, 'c] := (f, g) => (a : 'a, b : 'a) => f(g(a), g(b));

/// Return a predicate that is the logical negation of `pred`.
export let negate ['a] := (pred) => (x : 'a) => not pred(x);

/// Pipe `x` through `f` then `g`.
export let pipe2 ['a, 'b, 'c] := (x : 'a, f, g) => g(f(x));

/// Pipe `x` through `f`, `g`, then `h`.
export let pipe3 ['a, 'b, 'c, 'd] := (x : 'a, f, g, h) => h(g(f(x)));
