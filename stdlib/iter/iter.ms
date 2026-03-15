//! Lazy iterators for Musi.
//!
//! Each combinator wraps an inner iterator and transforms `next` calls.
//! All combinators are pure Musi — no builtins needed beyond array access.

import "musi:core" as core;

// ── Core iterator type ───────────────────────────────────────

export let Iter ['a] := record { next : () -> ?'a };

// ── Constructors ─────────────────────────────────────────────

export let from_array ['a] := (arr) => (
  let mut i := 0;
  let len := core.arr_len(arr);
  .{ next := () => (
    (let idx := i; i <- i + 1; .Some(arr.[idx])) if i < len
    | .None if _
  ) }
);

export let range := (start, end) => (
  let mut cur := start;
  .{ next := () => (
    (let v := cur; cur <- cur + 1; .Some(v)) if cur < end
    | .None if _
  ) }
);

export let repeat ['a] := (value) =>
  .{ next := () => .Some(value) };

export let empty ['a] := () =>
  .{ next := () => .None };

// ── Transformers ─────────────────────────────────────────────

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

// ── Consumers ────────────────────────────────────────────────

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
      .Some(x) => (core.arr_push(result, x); step())
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
