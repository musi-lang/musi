//! Semantic analysis.
//!
//! Current scope:
//! - AST to HIR lowering
//! - first-pass lexical name resolution
//! - basic type/effect checking over resolved HIR (gradual types via `Any`/`Unknown`)

mod errors;
mod checker;

pub use errors::{SemaError, SemaErrorKind, SemaErrorKinds};
pub use checker::{AnalyzedModule, analyze_module};
