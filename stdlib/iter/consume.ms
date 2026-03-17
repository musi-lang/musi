//! Iterator consumption (terminal) operations.

import "@std/rt" as rt;

export let fold ['a, 'b] := (iter, init, f) => (
  let mut acc := init;
  let step := () => (
    let v := iter.next();
    match v (
      .Some(x) => (acc <- f(acc, x); step())
      | .None => acc
    )
  );
  step()
);

export let collect_array ['a] := (iter) => (
  let mut result := [];
  let step := () => (
    let v := iter.next();
    match v (
      .Some(x) => (rt.arr_push(result, x); step())
      | .None => result
    )
  );
  step()
);

export let for_each ['a] := (iter, f) => (
  let step := () => (
    let v := iter.next();
    match v (
      .Some(x) => (f(x); step())
      | .None => ()
    )
  );
  step()
);

export let count ['a] := (iter) => (
  let mut n := 0;
  let step := () => (
    let v := iter.next();
    match v (
      .Some(_) => (n <- n + 1; step())
      | .None => n
    )
  );
  step()
);

export let any ['a] := (iter, pred) => (
  let step := () => (
    let v := iter.next();
    match v (
      .Some(x) => (.True if pred(x) | step() if _)
      | .None => .False
    )
  );
  step()
);

export let all ['a] := (iter, pred) => (
  let step := () => (
    let v := iter.next();
    match v (
      .Some(x) => (step() if pred(x) | .False if _)
      | .None => .True
    )
  );
  step()
);

export let find ['a] := (iter, pred) => (
  let step := () => (
    let v := iter.next();
    match v (
      .Some(x) => (.Some(x) if pred(x) | step() if _)
      | .None => .None
    )
  );
  step()
);

export let sum := (iter) => fold(iter, 0, (acc, x) => acc + x);
