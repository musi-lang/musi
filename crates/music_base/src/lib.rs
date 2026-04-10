//! Compiler foundation types.
//!
//! `music_base` owns:
//! - spans (`Span`, `Spanned<T>`)
//! - sources (`Source`, `SourceId`, `SourceMap`)
//! - diagnostics (`Diag*` types and `music_base::diag::emit` formatting)

pub mod diag;
pub mod source;
pub mod span;

pub use diag::{Diag, DiagCode, DiagLabel, DiagLevel};
pub use source::{Source, SourceId, SourceMap, SourceMapError};
pub use span::{Span, Spanned};
