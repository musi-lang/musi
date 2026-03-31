mod model;
mod render;
mod style;

pub use model::{Diag, DiagCode, DiagLabel, DiagLevel};
pub use render::{DiagColor, emit, emit_to_stderr, supports_color};

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;

