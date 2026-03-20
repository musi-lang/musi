//! Lazy iterators.

/// Iterable type class.
export let iterable := import "./iterable";
/// Iterator source constructors.
export let sources := import "./sources";
/// Iterator transformation combinators.
export let transform := import "./transform";
/// Iterator terminal (consuming) operations.
export let consume := import "./consume";
