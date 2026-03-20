//! Iterator consumption (terminal) operations.

import "@std/rt" as rt;

/// Reduce the iterator left-to-right using `f`, starting from `init`.
export let fold ['a, 'b] := (iter, init : 'b, f) => (
  let mut acc : 'b := init;
  let step := () => (
    let v : ?'a := iter.next();
    match v (
      .Some(x : 'a) => (acc <- f(acc, x); step())
      | .None => acc
    )
  );
  step()
);

/// Collect all iterator elements into an array.
export let collect_array ['a] := (iter) => (
  let mut result := [];
  let step := () => (
    let v : ?'a := iter.next();
    match v (
      .Some(x : 'a) => (rt.arr_push(result, x); step())
      | .None => result
    )
  );
  step()
);

/// Call `f` on each element for its side effects.
export let for_each ['a] := (iter, f) => (
  let step := () => (
    let v : ?'a := iter.next();
    match v (
      .Some(x : 'a) => (f(x); step())
      | .None => ()
    )
  );
  step()
);

/// Return the number of elements yielded by the iterator.
export let count ['a] := (iter) => (
  let mut n := 0;
  let step := () => (
    let v : ?'a := iter.next();
    match v (
      .Some(_) => (n <- n + 1; step())
      | .None => n
    )
  );
  step()
);

/// Return `.True` if any element satisfies `pred`.
export let any ['a] := (iter, pred) => (
  let step := () => (
    let v : ?'a := iter.next();
    match v (
      .Some(x : 'a) => (.True if pred(x) | step() if _)
      | .None => .False
    )
  );
  step()
);

/// Return `.True` if every element satisfies `pred`.
export let all ['a] := (iter, pred) => (
  let step := () => (
    let v : ?'a := iter.next();
    match v (
      .Some(x : 'a) => (step() if pred(x) | .False if _)
      | .None => .True
    )
  );
  step()
);

/// Return the first element satisfying `pred`, or `.None`.
export let find ['a] := (iter, pred) => (
  let step := () => (
    let v : ?'a := iter.next();
    match v (
      .Some(x : 'a) => (.Some(x) if pred(x) | step() if _)
      | .None => .None
    )
  );
  step()
);

/// Return the sum of all elements.
export let sum := (iter) => fold(iter, 0, (acc, x) => acc + x);
