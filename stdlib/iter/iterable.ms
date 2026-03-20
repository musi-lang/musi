//! Iterable type class.

/// Iteration protocol: types that can produce a sequence of elements.
#[lang := "Iterable"]
export class Iterable ['a, 'elem] {
  let next(iter) : ?'elem;
};
