//! Iterator transformation combinators.

export let map ['a, 'b] := (iter, f) =>
  .{ next := () => (
    let v := iter.next();
    match v (
      .Some(x) => .Some(f(x))
      | .None => .None
    )
  ) };

export let filter ['a] := (iter, pred) => (
  let step := () => (
    let v := iter.next();
    match v (
      .Some(x) => (.Some(x) if pred(x) | step() if _)
      | .None => .None
    )
  );
  .{ next := step }
);

export let take ['a] := (iter, n) => (
  let mut remaining := n;
  .{ next := () => (
    (remaining <- remaining - 1; iter.next()) if remaining > 0
    | .None if _
  ) }
);

export let skip ['a] := (iter, n) => (
  let mut skipped := 0;
  let step := () => (
    (iter.next(); skipped <- skipped + 1; step()) if skipped < n
    | iter.next() if _
  );
  .{ next := step }
);

export let zip ['a, 'b] := (a, b) =>
  .{ next := () => (
    let va := a.next();
    match va (
      .Some(x) => (
        let vb := b.next();
        match vb (
          .Some(y) => .Some((x, y))
          | .None => .None
        )
      )
      | .None => .None
    )
  ) };

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
