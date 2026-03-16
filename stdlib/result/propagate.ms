//! Propagate type class for error propagation.

#[lang := "Propagate"]
export class Propagate ['e] {
  let propagate(e) : Never;
};
