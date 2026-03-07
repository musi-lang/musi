export choice List['T] { Nil | Cons('T, List['T]) };

export fn list_length['T](xs: List['T]): Int => match xs with (
    .Nil        => 0
  | .Cons(_, t) => 1 + list_length(t)
);

export fn list_append['T](xs: List['T], ys: List['T]): List['T] => match xs with (
    .Nil        => ys
  | .Cons(h, t) => .Cons(h, list_append(t, ys))
);

fn list_rev_aux['T](acc: List['T], rest: List['T]): List['T] => match rest with (
    .Nil        => acc
  | .Cons(h, t) => list_rev_aux(.Cons(h, acc), t)
);

export fn list_reverse['T](xs: List['T]): List['T] => list_rev_aux(.Nil, xs);

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

export fn list_head['T](xs: List['T]): Option['T] => match xs with (
    .Nil        => .None
  | .Cons(h, _) => .Some(h)
);

export fn list_tail['T](xs: List['T]): Option[List['T]] => match xs with (
    .Nil        => .None
  | .Cons(_, t) => .Some(t)
);

export fn list_any['T](xs: List['T], pred: 'T -> Bool): Bool => match xs with (
    .Nil        => .False
  | .Cons(h, t) => if pred(h) then .True else list_any(t, pred)
);

export fn list_all['T](xs: List['T], pred: 'T -> Bool): Bool => match xs with (
    .Nil        => .True
  | .Cons(h, t) => if pred(h) then list_all(t, pred) else .False
);

export fn list_sum(xs: List[Int]): Int => list_fold(xs, 0, fn(acc: Int, x: Int): Int => acc + x);

// Short-name aliases
export fn length['T](xs: List['T]): Int => list_length(xs);
export fn head['T](xs: List['T]): Option['T] => list_head(xs);
export fn tail['T](xs: List['T]): Option[List['T]] => list_tail(xs);
