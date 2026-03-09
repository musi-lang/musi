//! Shared infrastructure for the Musi compiler.
//!
//! Every other crate in the workspace depends on this one.  It provides:
//!
//! - [`Span`] -- a byte-range source location
//! - [`FileId`] / [`SourceDb`] -- file registry with line/column lookup
//! - [`Symbol`] / [`Interner`] -- arena-backed string deduplication
//! - [`Diagnostic`] / [`DiagnosticBag`] -- structured, accumulating error reporting

pub mod arena;
pub mod diag;
pub mod intern;
pub mod source;
pub mod span;

pub use arena::{Arena, Idx, IdxRange};
pub use diag::{Diagnostic, DiagnosticBag, IntoDiagnostic, Label, Severity};
pub use intern::{Interner, Symbol};
pub use source::{FileId, SourceDb, SourceFile};
pub use span::Span;
