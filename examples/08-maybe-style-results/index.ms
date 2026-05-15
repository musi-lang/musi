let io := import "@std/io";

export hidden let MaybeInt := data {
  | None()
  | Some(value : Int)
};

export let plusOne (value : MaybeInt) : MaybeInt :=
  match value (
  | .None() => .None()
  | .Some(value := inner) => .Some(value := inner + 1)
  );

export let unwrapOr (value : MaybeInt, fallback : Int) :=
  match value (
  | .None() => fallback
  | .Some(value := inner) => inner
  );

export let demo () :=
  (unwrapOr(plusOne(.Some(value := 41)), 0), unwrapOr(plusOne(.None()), 99));

demo();
io.writeLn("maybe: Some and None paths evaluated");
