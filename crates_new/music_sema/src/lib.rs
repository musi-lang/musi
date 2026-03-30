//! Semantic analysis.
//!
//! Current scope:
//! - AST to HIR lowering
//! - first-pass lexical name resolution (no type/effect checking yet)

mod errors;
pub mod resolve;

pub use errors::{SemaError, SemaErrorKind};
pub use resolve::{ResolveOptions, ResolvedModule, resolve_module};
