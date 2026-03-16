//! Propagate type class for error propagation.

#[lang := "Propagate"]
class Propagate ['e] {
  let propagate(e) : Never;
};
