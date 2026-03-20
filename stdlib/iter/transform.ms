//! Iterator transformation combinators.

/// Apply `f` to each element, yielding transformed values.
export let map ['a, 'b] := (iter, f) =>
  .{ next := () => (
    let v : ?'a := iter.next();
    match v (
      .Some(x : 'a) => .Some(f(x))
      | .None => .None
    )
  ) };

/// Yield only elements for which `pred` returns `.True`.
export let filter ['a] := (iter, pred) => (
  let step := () => (
    let v : ?'a := iter.next();
    match v (
      .Some(x : 'a) => (.Some(x) if pred(x) | step() if _)
      | .None => .None
    )
  );
  .{ next := step }
);

/// Yield at most `n` elements from `iter`.
export let take ['a] := (iter, n) => (
  let mut remaining := n;
  .{ next := () => (
    (remaining <- remaining - 1; iter.next()) if remaining > 0
    | .None if _
  ) }
);

/// Skip the first `n` elements, then yield the rest.
export let skip ['a] := (iter, n) => (
  let mut skipped := 0;
  let step := () => (
    (iter.next(); skipped <- skipped + 1; step()) if skipped < n
    | iter.next() if _
  );
  .{ next := step }
);

/// Yield pairs of corresponding elements, stopping when either iterator ends.
export let zip ['a, 'b] := (a, b) =>
  .{ next := () => (
    let va : ?'a := a.next();
    match va (
      .Some(x : 'a) => (
        let vb : ?'b := b.next();
        match vb (
          .Some(y : 'b) => .Some((x, y))
          | .None => .None
        )
      )
      | .None => .None
    )
  ) };

/// Yield all elements of `a` followed by all elements of `b`.
export let chain ['a] := (a, b) => (
  let mut exhausted_a := .False;
  .{ next := () => (
    (
      let va := a.next();
      match va (
        .Some(x) => .Some(x)
        | .None => (exhausted_a <- .True; b.next())
      )
    ) if not exhausted_a
    | b.next() if _
  ) }
);

/// Pair each element with its zero-based index.
export let enumerate ['a] := (iter) => (
  let mut i := 0;
  .{ next := () => (
    let v := iter.next();
    match v (
      .Some(x) => (let idx := i; i <- i + 1; .Some((idx, x)))
      | .None => .None
    )
  ) }
);

/// Map each element to an iterator and yield all elements of each sub-iterator in order.
export let flat_map ['a, 'b] := (iter, f) => (
  let mut inner := .None;
  let step := () =>
    match inner (
      .Some(cur) => (
        let v := cur.next();
        match v (
          .Some(x) => .Some(x)
          | .None => (inner <- .None; step())
        )
      )
      | .None => (
        let v := iter.next();
        match v (
          .Some(x) => (inner <- .Some(f(x)); step())
          | .None => .None
        )
      )
    );
  .{ next := step }
);

/// Yield elements while `pred` returns `.True`, then stop.
export let take_while ['a] := (iter, pred) => (
  let mut done := .False;
  .{ next := () => (
    (
      let v := iter.next();
      match v (
        .Some(x) => (.Some(x) if pred(x) | (done <- .True; .None) if _)
        | .None => .None
      )
    ) if not done
    | .None if _
  ) }
);

/// Skip leading elements while `pred` returns `.True`, then yield the rest.
export let skip_while ['a] := (iter, pred) => (
  let mut skipping := .True;
  let step := () => (
    let v := iter.next();
    match v (
      .Some(x) => (
        (step() if pred(x) | (skipping <- .False; .Some(x)) if _) if skipping
        | .Some(x) if _
      )
      | .None => .None
    )
  );
  .{ next := step }
);
