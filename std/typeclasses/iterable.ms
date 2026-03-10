/// Iteration protocol. `next` returns the next element or `None` when
/// exhausted. The `State` effect tracks iterator position.
export class Iterable over 'T {
    let next(iter: 'T) : ?'T ~> under { State };
};

/// Apply a function to each element, collecting results into a list.
export let map ['A, 'B] where 'A <: Iterable : ('A, ('A) -> 'B) -> List of 'B :=
    (iter, f) => (
        let go(acc) := match (next(iter)) (
            .Some(x) => go(.Cons(f(x), acc))
          | .None => reverse(acc)
        );
        go(.Nil)
    );

/// Keep only elements satisfying the predicate.
export let filter ['A] where 'A <: Iterable : ('A, ('A) -> Bool) -> List of 'A :=
    (iter, p) => (
        let go(acc) := match (next(iter)) (
            .Some(x) => go((.Cons(x, acc) if p(x) | acc if _))
          | .None => reverse(acc)
        );
        go(.Nil)
    );

/// Left fold over an iterable.
export let fold ['A, 'B] where 'A <: Iterable : ('A, 'B, ('B, 'A) -> 'B) -> 'B :=
    (iter, init, f) => (
        let go(acc) := match (next(iter)) (
            .Some(x) => go(f(acc, x))
          | .None => acc
        );
        go(init)
    );

/// Execute a side-effecting function on each element.
export let each ['A] where 'A <: Iterable : ('A, ('A) -> ()) ~> () under { IO } :=
    (iter, f) => (
        let go() := match (next(iter)) (
            .Some(x) => (f(x); go())
          | .None => ()
        );
        go()
    );

/// Count the number of elements.
export let count ['A] where 'A <: Iterable : ('A) -> Int :=
    (iter) => (
        let go(n) := match (next(iter)) (
            .Some(_) => go(n + 1)
          | .None => n
        );
        go(0)
    );

/// True if any element satisfies the predicate.
export let any ['A] where 'A <: Iterable : ('A, ('A) -> Bool) -> Bool :=
    (iter, p) => (
        let go() := match (next(iter)) (
            .Some(x) => (.True if p(x) | go() if _)
          | .None => .False
        );
        go()
    );

/// True if all elements satisfy the predicate.
export let all ['A] where 'A <: Iterable : ('A, ('A) -> Bool) -> Bool :=
    (iter, p) => (
        let go() := match (next(iter)) (
            .Some(x) => (go() if p(x) | .False if _)
          | .None => .True
        );
        go()
    );

/// Find the first element satisfying the predicate, or `None`.
export let find ['A] where 'A <: Iterable : ('A, ('A) -> Bool) -> ?'A :=
    (iter, p) => (
        let go() := match (next(iter)) (
            .Some(x) => (.Some(x) if p(x) | go() if _)
          | .None => .None
        );
        go()
    );
