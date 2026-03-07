// std/pair.ms — Pair['A, 'B] product type
// import { Pair, new, first, second, swap, map_first, map_second } from "std/pair"

export record Pair['A, 'B] { first: 'A, second: 'B };

export fn new['A, 'B](a: 'A, b: 'B): Pair['A, 'B] => Pair.{ first := a, second := b };

export fn first['A, 'B](p: Pair['A, 'B]): 'A => p.first;

export fn second['A, 'B](p: Pair['A, 'B]): 'B => p.second;

export fn swap['A, 'B](p: Pair['A, 'B]): Pair['B, 'A] => Pair.{ first := p.second, second := p.first };

export fn map_first['A, 'B, 'C](p: Pair['A, 'B], f: 'A -> 'C): Pair['C, 'B] =>
    Pair.{ first := f(p.first), second := p.second };

export fn map_second['A, 'B, 'C](p: Pair['A, 'B], f: 'B -> 'C): Pair['A, 'C] =>
    Pair.{ first := p.first, second := f(p.second) };
