//! Compiler foundation types.
//!
//! `music_base` owns:
//! - spans (`Span`, `Spanned<T>`)
//! - sources (`Source`, `SourceId`, `SourceMap`)
//! - diagnostics (`Diag*` types and `music_base::diag::emit` formatting)

pub mod diag;
pub mod int_literal;
pub mod source;
pub mod span;

pub use diag::{
    CatalogDiagnostic, Diag, DiagCode, DiagContext, DiagFix, DiagLabel, DiagLabelKind, DiagLevel,
    DiagnosticError, DiagnosticKind, OwnedSourceDiag,
};
pub use int_literal::{
    NumericSuffix, NumericSuffixClass, parse_i64_literal, parse_u32_literal, parse_u64_literal,
    split_numeric_suffix,
};
pub use source::{Source, SourceId, SourceMap, SourceMapError};
pub use span::{Span, Spanned};
