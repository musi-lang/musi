//! Ordering choice type.

/// The result of a comparison: Less means left < right, Equal means left = right, Greater means left > right.
#[lang := "Ordering"]
export let Ordering := choice { Less + Equal + Greater };
