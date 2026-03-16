//! Test assertion helpers.

export let assert_eq ['a] := (a, b) => a = b;

export let assert_ne ['a] := (a, b) => not (a = b);

export let assert_true := (b) => b;

export let assert_false := (b) => not b;
