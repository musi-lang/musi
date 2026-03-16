//! Iterable type class.

#[lang := "Iterable"]
class Iterable ['a, 'elem] {
  let next(iter) : ?'elem;
};
