//! Core primitives for Musi compiler.

pub mod arena;
pub mod diag;
pub mod error;
pub mod intern;
pub mod source;
pub mod span;
pub mod symbol;
pub mod token;

pub use arena::{Arena, NodeId};
pub use diag::{Diagnostic, DiagnosticBag, Level};
pub use error::{ErrorCode, MusiError, MusiResult};
pub use intern::Interner;
pub use source::{SourceFile, SourceMap};
pub use span::Span;
pub use symbol::Symbol;
pub use token::{Token, TokenKind};
