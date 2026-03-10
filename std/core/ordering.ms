/// Represents the result of a comparison: less than, equal, or greater than.
export let Ordering := Lt + Eq + Gt;

/// Chain two orderings: if the first is `Eq`, use the second.
export let then : (Ordering, Ordering) -> Ordering :=
    (a, b) -> match a (.Eq => b | _ => a);

/// Reverse an ordering.
export let reverse : Ordering -> Ordering :=
    (ord) -> match ord (.Lt => .Gt | .Eq => .Eq | .Gt => .Lt);

/// True if the ordering is `Lt`.
export let is_lt : Ordering -> Bool :=
    (ord) -> match ord (.Lt => .True | _ => .False);

/// True if the ordering is `Eq`.
export let is_eq : Ordering -> Bool :=
    (ord) -> match ord (.Eq => .True | _ => .False);

/// True if the ordering is `Gt`.
export let is_gt : Ordering -> Bool :=
    (ord) -> match ord (.Gt => .True | _ => .False);

given Eq of Ordering {
    let (=)(a: Ordering, b: Ordering) : Bool :=
        match (a, b) (
            (.Lt, .Lt) => .True
          | (.Eq, .Eq) => .True
          | (.Gt, .Gt) => .True
          | _ => .False
        )
};

given Show of Ordering {
    let show(value: Ordering) : String :=
        match value (.Lt => "Lt" | .Eq => "Eq" | .Gt => "Gt")
};
