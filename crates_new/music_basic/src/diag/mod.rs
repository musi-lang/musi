mod model;
mod render;

pub use model::{Diag, DiagCode, DiagLevel, Label};
pub use render::{Color, emit, emit_to_stderr, supports_color};

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
