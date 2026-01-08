//! Core primitives for Musi compiler.

pub mod arena;
pub mod diag;
pub mod error;
pub mod interner;
pub mod source;
pub mod span;
pub mod token;

pub use arena::{Arena, NodeId};
pub use diag::{Diagnostic, DiagnosticBag, Level};
pub use error::{ErrorCode, MusiError, MusiResult};
pub use interner::{Interner, Name};
pub use source::{SourceFile, SourceMap};
pub use span::Span;
pub use token::{Token, TokenKind};
