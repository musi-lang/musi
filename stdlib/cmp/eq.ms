//! Eq type class and primitive instances.

#[lang("Eq")]
class Eq ['a] {
  let (=)(a, b) : Bool;

  law reflexivity(x) := x = x;
  law symmetry(x, y) := (x = y) = (y = x);
};

instance Eq of Int {
  let (=)(a, b) : Bool := a = b;
};

instance Eq of Nat {
  let (=)(a, b) : Bool := a = b;
};

instance Eq of Float64 {
  let (=)(a, b) : Bool := a = b;
};

instance Eq of Float {
  let (=)(a, b) : Bool := a = b;
};

instance Eq of String {
  let (=)(a, b) : Bool := a = b;
};

instance Eq of Bool {
  let (=)(a, b) : Bool := a = b;
};

instance Eq of Rune {
  let (=)(a, b) : Bool := a = b;
};
