/// An optional value: either `Some(x)` or `None`.
///
/// Example:
///   let x := .Some(42);
///   let y := map(x, (n) -> n + 1)  -- .Some(43)
#[lang := "option"]
export let Option := forall 'T -> Some of 'T + None;

/// Apply a function to the contained value, or propagate `None`.
export let map : forall 'T, 'U -> (?'T, ('T) -> 'U) -> ?'U :=
    forall 'T, 'U -> (opt, f) -> match opt (.Some(x) => .Some(f(x)) | .None => .None);

/// Apply a function that returns an option, flattening the result.
export let flat_map : forall 'T, 'U -> (?'T, ('T) -> ?'U) -> ?'U :=
    forall 'T, 'U -> (opt, f) -> match opt (.Some(x) => f(x) | .None => .None);

/// Return the contained value or a default.
export let unwrap_or : forall 'T -> (?'T, 'T) -> 'T :=
    forall 'T -> (opt, default) -> match opt (.Some(x) => x | .None => default);

/// Return the contained value or compute a default.
export let unwrap_or_else : forall 'T -> (?'T, () -> 'T) -> 'T :=
    forall 'T -> (opt, f) -> match opt (.Some(x) => x | .None => f());

/// True if the option contains a value.
export let is_some : forall 'T -> (?'T) -> Bool :=
    forall 'T -> (opt) -> match opt (.Some(_) => .True | .None => .False);

/// True if the option is `None`.
export let is_none : forall 'T -> (?'T) -> Bool :=
    forall 'T -> (opt) -> match opt (.Some(_) => .False | .None => .True);

/// Return `None` if either is `None`, otherwise return the second.
export let and : forall 'T, 'U -> (?'T, ?'U) -> ?'U :=
    forall 'T, 'U -> (a, b) -> match a (.Some(_) => b | .None => .None);

/// Return the first `Some`, or `None` if both are `None`.
export let or : forall 'T -> (?'T, ?'T) -> ?'T :=
    forall 'T -> (a, b) -> match a (.Some(_) => a | .None => b);

/// Keep the value only if it satisfies the predicate.
export let filter : forall 'T -> (?'T, ('T) -> Bool) -> ?'T :=
    forall 'T -> (opt, p) -> match opt (
        .Some(x) => (.Some(x) if p(x) | .None if _)
      | .None => .None
    );

/// Flatten a nested option.
export let flatten : forall 'T -> (?(?'T)) -> ?'T :=
    forall 'T -> (opt) -> match opt (.Some(inner) => inner | .None => .None);

/// Combine two options into an option of a pair.
export let zip : forall 'T, 'U -> (?'T, ?'U) -> ?(('T, 'U)) :=
    forall 'T, 'U -> (a, b) -> match (a, b) (
        (.Some(x), .Some(y)) => .Some((x, y))
      | _ => .None
    );

given Eq of (Option of 'T) where 'T <: Eq {
    let (=)(a: ?'T, b: ?'T) : Bool :=
        match (a, b) (
            (.Some(x), .Some(y)) => x = y
          | (.None, .None) => .True
          | _ => .False
        )
};

given Show of (Option of 'T) where 'T <: Show {
    let show(value: ?'T) : String :=
        match value (.Some(x) => "Some(" + show(x) + ")" | .None => "None")
};

given Ord of (Option of 'T) where 'T <: Ord {
    let compare(a: ?'T, b: ?'T) : Ordering :=
        match (a, b) (
            (.None, .None) => .Eq
          | (.None, .Some(_)) => .Lt
          | (.Some(_), .None) => .Gt
          | (.Some(x), .Some(y)) => compare(x, y)
        )
};
