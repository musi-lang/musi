export const Str := record {
  ptr: Ptr<const Unit>,
  len: Nat,
};

export const Str.len := proc (s: Str) -> Nat {
  s.len
};

export const Str.is_empty := proc (s: Str) -> Bool {
  s.len = 0
};
