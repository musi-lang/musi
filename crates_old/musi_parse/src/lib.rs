//! LL(1) + Pratt parser for the Musi compiler.
//!
//! Converts a flat [`Token`] slice (from `musi_lex`) into a typed AST.
//! AST nodes are arena-allocated via [`AstArenas`]; clients hold [`Idx<Expr>`] handles.
//!
//! # Entry point
//!
//! ```ignore
//! let module = musi_parse::parse(&tokens, file_id, &mut diags, &interner);
//! ```

pub mod ast;
pub mod parser;
pub mod sexpr;

pub use ast::{AstArenas, ParsedModule};
pub use parser::parse;
