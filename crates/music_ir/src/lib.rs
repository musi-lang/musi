//! Typed IR surface used by lowering and codegen.
//!
//! This crate does not own parsing, name resolution, or type checking. It owns
//! compact, codegen-facing facts derived from semantic analysis.

mod layout;
mod module;
mod ty;

pub use layout::{IrDataLayout, IrDataLayouts};
pub use module::IrModuleInfo;
pub use ty::{IrExprTy, IrScalarTy, IrTypeRef};
