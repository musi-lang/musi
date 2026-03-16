//! Function combinators for Musi.

export let identity ['a] := (x) => x;

export let constant ['a, 'b] := (x) => (_) => x;

export let compose ['a, 'b, 'c] := (f, g) => (x) => f(g(x));

export let flip ['a, 'b, 'c] := (f) => (a, b) => f(b, a);

export let apply ['a, 'b] := (f, x) => f(x);

export let on ['a, 'b, 'c] := (f, g) => (a, b) => f(g(a), g(b));

export let negate ['a] := (pred) => (x) => not pred(x);

export let pipe2 ['a, 'b, 'c] := (x, f, g) => g(f(x));

export let pipe3 ['a, 'b, 'c, 'd] := (x, f, g, h) => h(g(f(x)));
