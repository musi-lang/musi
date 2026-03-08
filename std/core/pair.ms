// std/core/pair.ms — Pair['A, 'B] product type
// import { Pair, new, first, second, swap, map_first, map_second } from "std/core/pair"

/// A pair of two values of possibly different types.
export record Pair['A, 'B] { first: 'A, second: 'B };

/// Construct a `Pair` from two values.
export fn new['A, 'B](a: 'A, b: 'B): Pair['A, 'B] => Pair.{ first := a, second := b };

/// Extract the first element of a pair.
export fn first['A, 'B](p: Pair['A, 'B]): 'A => p.first;

/// Extract the second element of a pair.
export fn second['A, 'B](p: Pair['A, 'B]): 'B => p.second;

/// Swap the two elements of a pair.
export fn swap['A, 'B](p: Pair['A, 'B]): Pair['B, 'A] => Pair.{ first := p.second, second := p.first };

/// Apply `f` to the first element, leaving the second unchanged.
export fn map_first['A, 'B, 'C](p: Pair['A, 'B], f: 'A -> 'C): Pair['C, 'B] =>
    Pair.{ first := f(p.first), second := p.second };

/// Apply `f` to the second element, leaving the first unchanged.
export fn map_second['A, 'B, 'C](p: Pair['A, 'B], f: 'B -> 'C): Pair['A, 'C] =>
    Pair.{ first := p.first, second := f(p.second) };
