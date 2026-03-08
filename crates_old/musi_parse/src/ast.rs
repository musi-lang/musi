//! AST re-exports from `musi_ast`.
//!
//! All AST type definitions live in the `musi_ast` crate.
//! This module re-exports them for backward compatibility within
//! the `musi_parse` crate itself.

pub use musi_ast::*;

#[cfg(test)]
mod tests;
