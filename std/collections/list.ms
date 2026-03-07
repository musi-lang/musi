// std/collections/list.ms — singly-linked List['T] and utilities
// import { List, list_length, list_head, list_tail, list_map, list_filter, list_fold, ... } from "std/collections/list"

export choice List['T] { Nil | Cons('T, List['T]) };

export fn list_length['T](xs: List['T]): Int => match xs with (
    .Nil        => 0
  | .Cons(_, t) => 1 + list_length(t)
);

export fn list_is_empty['T](xs: List['T]): Bool => match xs with (
    .Nil => .True
  | _    => .False
);

export fn list_head['T](xs: List['T]): Option['T] => match xs with (
    .Nil        => .None
  | .Cons(h, _) => .Some(h)
);

export fn list_tail['T](xs: List['T]): Option[List['T]] => match xs with (
    .Nil        => .None
  | .Cons(_, t) => .Some(t)
);

export fn list_append['T](xs: List['T], ys: List['T]): List['T] => match xs with (
    .Nil        => ys
  | .Cons(h, t) => .Cons(h, list_append(t, ys))
);

fn reverse_aux['T](acc: List['T], rest: List['T]): List['T] => match rest with (
    .Nil        => acc
  | .Cons(h, t) => reverse_aux(.Cons(h, acc), t)
);

export fn list_reverse['T](xs: List['T]): List['T] => reverse_aux(.Nil, xs);

export fn list_map['T, 'U](xs: List['T], f: 'T -> 'U): List['U] => match xs with (
    .Nil        => .Nil
  | .Cons(h, t) => .Cons(f(h), list_map(t, f))
);

export fn list_filter['T](xs: List['T], pred: 'T -> Bool): List['T] => match xs with (
    .Nil        => .Nil
  | .Cons(h, t) => if pred(h) then .Cons(h, list_filter(t, pred)) else list_filter(t, pred)
);

export fn list_fold['T, 'U](xs: List['T], init: 'U, f: ('U, 'T) -> 'U): 'U => match xs with (
    .Nil        => init
  | .Cons(h, t) => list_fold(t, f(init, h), f)
);

export fn list_any['T](xs: List['T], pred: 'T -> Bool): Bool => match xs with (
    .Nil        => .False
  | .Cons(h, t) => if pred(h) then .True else list_any(t, pred)
);

export fn list_all['T](xs: List['T], pred: 'T -> Bool): Bool => match xs with (
    .Nil        => .True
  | .Cons(h, t) => if pred(h) then list_all(t, pred) else .False
);

export fn list_find['T](xs: List['T], pred: 'T -> Bool): Option['T] => match xs with (
    .Nil        => .None
  | .Cons(h, t) => if pred(h) then .Some(h) else list_find(t, pred)
);

export fn list_flat_map['T, 'U](xs: List['T], f: 'T -> List['U]): List['U] => match xs with (
    .Nil        => .Nil
  | .Cons(h, t) => list_append(f(h), list_flat_map(t, f))
);

export fn list_sum(xs: List[Int]): Int => list_fold(xs, 0, fn(acc: Int, x: Int): Int => acc + x);

export fn list_product(xs: List[Int]): Int => list_fold(xs, 1, fn(acc: Int, x: Int): Int => acc * x);
