// intrinsic def of choice type `Expect<T, E>`
// used for procedures that may return either value of type T or error of type E

// we may need an attribute here, or what?
// rust uses `#[lang("name")]` for similar purposes
// i assume we'd need something like `@intrinsic("musi_Expect")` or whatever?
// or maybe follow the way C++ does it with `__builtin_xxx` names?
// i've no fuckin idea. gotta figure that out or just gobble together a runtime-one?
// fuck... don't really wanna hardcode shit into runtime for this one purpose.
export const Expect := choice Expect<T, E> {
  case Pass(T),
  case Fail(E),
};

// are these even idiomatic?
export const Expect.is_pass := proc (exp: Expect<T, E>) -> Bool {
  // match exp {
  // case Pass(_) => true,
  // case Fail(_) => false
  // }
  // i'm so lazy i'm using single-lined if-then-else here
  if exp = .Pass(_) then true else false
};

export const Expect.is_fail := proc (exp: Expect<T, E>) -> Bool {
  if exp = .Fail(_) then true else false
};
