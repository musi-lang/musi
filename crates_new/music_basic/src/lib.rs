pub mod diag;
pub mod ident;
pub mod literal;
pub mod source;
pub mod span;
pub mod symbol;

pub use diag::{Color, Diag, DiagCode, DiagLevel, Label, emit, emit_to_stderr, supports_color};
pub use ident::Ident;
pub use literal::Literal;
pub use source::{Source, SourceId, SourceMap};
pub use span::{Span, Spanned};
pub use symbol::{Interner, Symbol};

pub type SymbolList = Vec<Symbol>;
