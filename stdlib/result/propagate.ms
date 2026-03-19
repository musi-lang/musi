//! Propagate type class for error propagation.

/// Error propagation contract: enables the `?` operator to short-circuit on failure.
#[lang := "Propagate"]
export class Propagate ['e] {
  let propagate(e) : Never;
};
