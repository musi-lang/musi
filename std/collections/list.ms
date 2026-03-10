/// A singly-linked immutable list.
export let List := forall 'T -> Nil + Cons of ('T, List of 'T);

/// Return the first element, or `None` if empty.
export let head : forall 'T -> (List of 'T) -> ?'T :=
    forall 'T -> (xs) -> match xs (.Cons(x, _) => .Some(x) | .Nil => .None);

/// Return all elements after the first, or `Nil` if empty.
export let tail : forall 'T -> (List of 'T) -> List of 'T :=
    forall 'T -> (xs) -> match xs (.Cons(_, rest) => rest | .Nil => .Nil);

/// True if the list is empty.
export let is_empty : forall 'T -> (List of 'T) -> Bool :=
    forall 'T -> (xs) -> match xs (.Nil => .True | _ => .False);

/// Return the number of elements.
export let length : forall 'T -> (List of 'T) -> Int :=
    forall 'T -> (xs) -> (
        let go(acc, ys) := match ys (.Cons(_, rest) => go(acc + 1, rest) | .Nil => acc);
        go(0, xs)
    );

/// Apply a function to each element.
export let map : forall 'T, 'U -> (List of 'T, ('T) -> 'U) -> List of 'U :=
    forall 'T, 'U -> (xs, f) -> match xs (
        .Cons(x, rest) => .Cons(f(x), map(rest, f))
      | .Nil => .Nil
    );

/// Keep only elements satisfying the predicate.
export let filter : forall 'T -> (List of 'T, ('T) -> Bool) -> List of 'T :=
    forall 'T -> (xs, p) -> match xs (
        .Cons(x, rest) => (
            .Cons(x, filter(rest, p)) if p(x)
          | filter(rest, p) if _
        )
      | .Nil => .Nil
    );

/// Left fold.
export let fold_left : forall 'T, 'U -> (List of 'T, 'U, ('U, 'T) -> 'U) -> 'U :=
    forall 'T, 'U -> (xs, acc, f) -> match xs (
        .Cons(x, rest) => fold_left(rest, f(acc, x), f)
      | .Nil => acc
    );

/// Right fold.
export let fold_right : forall 'T, 'U -> (List of 'T, 'U, ('T, 'U) -> 'U) -> 'U :=
    forall 'T, 'U -> (xs, acc, f) -> match xs (
        .Cons(x, rest) => f(x, fold_right(rest, acc, f))
      | .Nil => acc
    );

/// Reverse a list.
export let reverse : forall 'T -> (List of 'T) -> List of 'T :=
    forall 'T -> (xs) -> fold_left(xs, .Nil, (acc, x) -> .Cons(x, acc));

/// Concatenate two lists.
export let append : forall 'T -> (List of 'T, List of 'T) -> List of 'T :=
    forall 'T -> (xs, ys) -> match xs (
        .Cons(x, rest) => .Cons(x, append(rest, ys))
      | .Nil => ys
    );

/// Flatten a list of lists.
export let flatten : forall 'T -> (List of (List of 'T)) -> List of 'T :=
    forall 'T -> (xss) -> fold_right(xss, .Nil, (xs, acc) -> append(xs, acc));

/// Return the element at index `n`, or `None` if out of bounds.
export let nth : forall 'T -> (List of 'T, Int) -> ?'T :=
    forall 'T -> (xs, n) -> match xs (
        .Cons(x, rest) => (.Some(x) if n = 0 | nth(rest, n - 1) if _)
      | .Nil => .None
    );

/// Zip two lists into a list of pairs, truncating to the shorter.
export let zip : forall 'T, 'U -> (List of 'T, List of 'U) -> List of ('T, 'U) :=
    forall 'T, 'U -> (xs, ys) -> match (xs, ys) (
        (.Cons(x, xr), .Cons(y, yr)) => .Cons((x, y), zip(xr, yr))
      | _ => .Nil
    );

/// Take the first `n` elements.
export let take : forall 'T -> (List of 'T, Int) -> List of 'T :=
    forall 'T -> (xs, n) -> (
        .Nil if n <= 0
      | match xs (
            .Cons(x, rest) => .Cons(x, take(rest, n - 1))
          | .Nil => .Nil
        ) if _
    );

/// Drop the first `n` elements.
export let drop : forall 'T -> (List of 'T, Int) -> List of 'T :=
    forall 'T -> (xs, n) -> (
        xs if n <= 0
      | match xs (
            .Cons(_, rest) => drop(rest, n - 1)
          | .Nil => .Nil
        ) if _
    );

/// True if the list contains the given element.
export let contains : forall 'T where 'T <: Eq -> (List of 'T, 'T) -> Bool :=
    forall 'T where 'T <: Eq -> (xs, value) -> match xs (
        .Cons(x, rest) => (.True if x = value | contains(rest, value) if _)
      | .Nil => .False
    );

given Eq of (List of 'T) where 'T <: Eq {
    let (=)(a: List of 'T, b: List of 'T) : Bool :=
        match (a, b) (
            (.Nil, .Nil) => .True
          | (.Cons(x, xr), .Cons(y, yr)) => (x = y) and (xr = yr)
          | _ => .False
        )
};

given Show of (List of 'T) where 'T <: Show {
    let show(value: List of 'T) : String := (
        let go(xs, acc) := match xs (
            .Cons(x, .Nil) => acc + show(x)
          | .Cons(x, rest) => go(rest, acc + show(x) + ", ")
          | .Nil => acc
        );
        "[" + go(value, "") + "]"
    )
};
