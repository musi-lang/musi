/// A singly-linked immutable list.
///
/// `List` is a persistent, functional data structure built from `Cons` cells.
/// The empty list is `Nil`.
///
/// # Examples
///
/// ```musi
/// let xs := .Cons(1, .Cons(2, .Cons(3, .Nil)));
/// let ys := map(xs, (n) -> n * 2);  // [2, 4, 6]
/// ```
export let List ['T] := choice { Nil + Cons of ('T, List of 'T) };

/// Returns the first element, or `None` if the list is empty.
export let head ['T] : (List of 'T) -> ?'T :=
    (xs) => match xs (.Cons(x, _) => .Some(x) | .Nil => .None);

/// Returns all elements after the first, or `Nil` if empty.
export let tail ['T] : (List of 'T) -> List of 'T :=
    (xs) => match xs (.Cons(_, rest) => rest | .Nil => .Nil);

/// Returns `True` if the list has no elements.
export let is_empty ['T] : (List of 'T) -> Bool :=
    (xs) => match xs (.Nil => .True | _ => .False);

/// Returns the number of elements in the list.
export let length ['T] : (List of 'T) -> Int :=
    (xs) => (
        let go(acc, ys) := match ys (.Cons(_, rest) => go(acc + 1, rest) | .Nil => acc);
        go(0, xs)
    );

/// Applies a function to each element, returning a new list of results.
export let map ['T, 'U] : (List of 'T, ('T) -> 'U) -> List of 'U :=
    (xs, f) => match xs (
        .Cons(x, rest) => .Cons(f(x), map(rest, f))
      | .Nil => .Nil
    );

/// Returns a new list containing only elements that satisfy the predicate.
export let filter ['T] : (List of 'T, ('T) -> Bool) -> List of 'T :=
    (xs, p) => match xs (
        .Cons(x, rest) => (
            .Cons(x, filter(rest, p)) if p(x)
          | filter(rest, p) if _
        )
      | .Nil => .Nil
    );

/// Reduces the list from the left using an accumulator and a combining function.
///
/// # Examples
///
/// ```musi
/// let xs := .Cons(1, .Cons(2, .Cons(3, .Nil)));
/// let sum := fold_left(xs, 0, (acc, x) -> acc + x);  // 6
/// ```
export let fold_left ['T, 'U] : (List of 'T, 'U, ('U, 'T) -> 'U) -> 'U :=
    (xs, acc, f) => match xs (
        .Cons(x, rest) => fold_left(rest, f(acc, x), f)
      | .Nil => acc
    );

/// Reduces the list from the right using an accumulator and a combining function.
///
/// Unlike `fold_left`, the combining function receives the element first
/// and the accumulator second, and association is right-to-left.
///
/// # Examples
///
/// ```musi
/// let xs := .Cons(1, .Cons(2, .Cons(3, .Nil)));
/// let result := fold_right(xs, .Nil, (x, acc) -> .Cons(x, acc));  // [1, 2, 3]
/// ```
export let fold_right ['T, 'U] : (List of 'T, 'U, ('T, 'U) -> 'U) -> 'U :=
    (xs, acc, f) => match xs (
        .Cons(x, rest) => f(x, fold_right(rest, acc, f))
      | .Nil => acc
    );

/// Returns a new list with elements in reverse order.
export let reverse ['T] : (List of 'T) -> List of 'T :=
    (xs) => fold_left(xs, .Nil, (acc, x) => .Cons(x, acc));

/// Concatenates two lists, placing all elements of the second after the first.
export let append ['T] : (List of 'T, List of 'T) -> List of 'T :=
    (xs, ys) => match xs (
        .Cons(x, rest) => .Cons(x, append(rest, ys))
      | .Nil => ys
    );

/// Flattens a list of lists into a single list.
export let flatten ['T] : (List of (List of 'T)) -> List of 'T :=
    (xss) => fold_right(xss, .Nil, (xs, acc) => append(xs, acc));

/// Returns the element at zero-based index `n`, or `None` if out of bounds.
///
/// # Examples
///
/// ```musi
/// let xs := .Cons(10, .Cons(20, .Cons(30, .Nil)));
/// nth(xs, 1)  // .Some(20)
/// nth(xs, 5)  // .None
/// ```
export let nth ['T] : (List of 'T, Int) -> ?'T :=
    (xs, n) => match xs (
        .Cons(x, rest) => (.Some(x) if n = 0 | nth(rest, n - 1) if _)
      | .Nil => .None
    );

/// Combines two lists element-wise into a list of pairs, truncating to the shorter.
///
/// # Examples
///
/// ```musi
/// let names := .Cons("a", .Cons("b", .Nil));
/// let nums  := .Cons(1, .Cons(2, .Cons(3, .Nil)));
/// zip(names, nums)  // [("a", 1), ("b", 2)]
/// ```
export let zip ['T, 'U] : (List of 'T, List of 'U) -> List of ('T, 'U) :=
    (xs, ys) => match (xs, ys) (
        (.Cons(x, xr), .Cons(y, yr)) => .Cons((x, y), zip(xr, yr))
      | _ => .Nil
    );

/// Returns the first `n` elements of the list.
export let take ['T] : (List of 'T, Int) -> List of 'T :=
    (xs, n) => (
        .Nil if n <= 0
      | match xs (
            .Cons(x, rest) => .Cons(x, take(rest, n - 1))
          | .Nil => .Nil
        ) if _
    );

/// Returns the list with the first `n` elements removed.
export let drop ['T] : (List of 'T, Int) -> List of 'T :=
    (xs, n) => (
        xs if n <= 0
      | match xs (
            .Cons(_, rest) => drop(rest, n - 1)
          | .Nil => .Nil
        ) if _
    );

/// Returns `True` if the list contains the given element.
export let contains ['T] where 'T <: Eq : (List of 'T, 'T) -> Bool :=
    (xs, value) => match xs (
        .Cons(x, rest) => (.True if x = value | contains(rest, value) if _)
      | .Nil => .False
    );

given Eq of (List of 'T) where 'T <: Eq {
    let (=)(a: List of 'T, b: List of 'T) : Bool :=
        match (a, b) (
            (.Nil, .Nil) => .True
          | (.Cons(x, xr), .Cons(y, yr)) => (x = y) and (xr = yr)
          | _ => .False
        );
};

given Show of (List of 'T) where 'T <: Show {
    let show(value: List of 'T) : String := (
        let go(xs, acc) := match xs (
            .Cons(x, .Nil) => acc + show(x)
          | .Cons(x, rest) => go(rest, acc + show(x) + ", ")
          | .Nil => acc
        );
        "[" + go(value, "") + "]"
    );
};
