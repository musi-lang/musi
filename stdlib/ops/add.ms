//! Add type class and primitive instances.

#[lang("Add")]
class Add ['a] {
  let (+)(a, b) : 'a;
};

instance Add of Int {
  let (+)(a, b) : Int := a + b;
};

instance Add of Nat {
  let (+)(a, b) : Nat := a + b;
};

instance Add of Float64 {
  let (+)(a, b) : Float64 := a + b;
};

instance Add of Float {
  let (+)(a, b) : Float := a + b;
};

instance Add of String {
  let (+)(a, b) : String := a + b;
};
