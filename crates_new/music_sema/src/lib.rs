//! Semantic analysis.
//!
//! Current scope:
//! - AST to HIR lowering
//! - first-pass lexical name resolution
//! - basic type/effect checking over resolved HIR (gradual types via `Any`/`Unknown`)

mod checker;
mod errors;

pub use checker::{AnalyzedModule, analyze_module};
pub use errors::{SemaError, SemaErrorKind, SemaErrorKinds, SemaErrors};
