//! LL(1) + Pratt parser for the Musi compiler.
//!
//! Converts a flat [`Token`] slice (from `musi_lex`) into a typed AST.
//! AST nodes are arena-allocated via [`ParseCtx`]; clients hold [`Idx<Expr>`] handles.
//!
//! # Entry point
//!
//! ```ignore
//! let module = musi_parse::parse(&tokens, file_id, &mut diags, &interner);
//! ```

#![allow(clippy::module_name_repetitions)]
#![allow(clippy::exhaustive_structs)]
#![allow(clippy::exhaustive_enums)]

pub mod ast;
pub mod parser;
pub mod sexpr;

pub use ast::{ParseCtx, ParsedModule};
pub use parser::parse;
