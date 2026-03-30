//! Module resolution and AST to HIR lowering.
//!
//! Owns:
//! - import existence checks (via `ImportEnv`)
//! - lexical scope and binding graphs
//! - export collection
//! - AST to HIR lowering
//!
//! Does not own:
//! - type checking
//! - effect checking

mod errors;
pub mod resolve;

pub use errors::{ResolveError, ResolveErrorKind};
pub use resolve::{ImportEnv, ResolveOptions, ResolvedModule, resolve_module};

