//! Comparison: Eq, Ord, Ordering, and comparator combinators.

/// Equality type class and primitive instances.
export let eq := import "./eq";
/// Total ordering type class and primitive instances.
export let ord := import "./ord";
/// The Ordering choice type (Less, Equal, Greater).
export let ordering := import "./ordering";
/// Utilities for working with Ordering values and comparator functions.
export let compare := import "./compare";
