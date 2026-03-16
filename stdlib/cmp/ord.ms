//! Ord type class and primitive instances.

#[lang := "Ord"]
class Ord ['a] where 'a <: Eq {
  let compare(a, b) : Ordering;
};

instance Ord of Int {
  let compare(a, b) : Ordering := (
    .Less if a < b
    | .Greater if a > b
    | .Equal if _
  );
};

instance Ord of Nat {
  let compare(a, b) : Ordering := (
    .Less if a < b
    | .Greater if a > b
    | .Equal if _
  );
};

instance Ord of String {
  let compare(a, b) : Ordering := (
    .Less if a < b
    | .Greater if a > b
    | .Equal if _
  );
};

instance Ord of Rune {
  let compare(a, b) : Ordering := (
    .Less if a < b
    | .Greater if a > b
    | .Equal if _
  );
};
