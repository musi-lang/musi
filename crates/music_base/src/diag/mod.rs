mod catalog;
mod model;
mod render;
mod style;

pub use catalog::{
    CatalogDiagnostic, DiagContext, DiagnosticError, DiagnosticKind, display_catalog_or_source,
};
pub use model::{Diag, DiagCode, DiagFix, DiagLabel, DiagLabelKind, DiagLevel, OwnedSourceDiag};
pub use render::{DiagColor, emit, emit_to_stderr, supports_color};

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
