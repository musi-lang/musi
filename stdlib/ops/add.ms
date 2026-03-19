//! Add type class and primitive instances.

/// Additive operator: types that support the `+` operator.
#[lang := "Add"]
export class Add ['a] {
  let (+)(a, b) : 'a;
};

export instance Add of Int {
  let (+)(a, b) : Int := a + b;
};

export instance Add of Nat {
  let (+)(a, b) : Nat := a + b;
};

export instance Add of Float64 {
  let (+)(a, b) : Float64 := a + b;
};

export instance Add of Float {
  let (+)(a, b) : Float := a + b;
};

export instance Add of String {
  let (+)(a, b) : String := a + b;
};
