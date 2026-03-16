//! Show type class and primitive instances.

#[lang := "Show"]
class Show ['a] {
  let show(a) : String;
};

instance Show of Int {
  let show(a) : String := show(a);
};

instance Show of Nat {
  let show(a) : String := show(a);
};

instance Show of Float64 {
  let show(a) : String := show(a);
};

instance Show of Float {
  let show(a) : String := show(a);
};

instance Show of Bool {
  let show(a) : String := show(a);
};

instance Show of String {
  let show(a) : String := a;
};

instance Show of Rune {
  let show(a) : String := show(a);
};
