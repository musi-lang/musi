//! Orchestrator for lowering AST + sema results into MSIR.
//!
//! The lowering pipeline runs these passes in order:
//!
//! 1. **Initial lowering** — walk AST with `SemaResult` to produce raw ANF IR
//! 2. **Desugaring** — pipe, try, nil-coalesce, defer, f-string, partial app
//! 3. **Closure conversion** — free-variable analysis, env struct, capture rewriting
//! 4. **Monomorphization / Existentialization** — specialize or generalize generics
//! 5. **Inout lowering** — convert `inout` params to pointer-passing
//! 6. **Effect lowering** — semantic effect rows to IR effect masks and handler ops
//! 7. **Tail-call identification** — mark tail-position calls

pub mod closure;
pub mod decl;
pub mod desugar;
pub mod effect;
pub mod exist;
pub mod expr;
pub mod inout;
pub mod mono;
pub mod pat;
pub mod tail;
pub mod ty;

use music_ast::ParsedModule;
use music_sema::SemaResult;
use music_shared::Interner;

use crate::IrModule;
use crate::error::IrError;

/// Lowers a parsed module + sema result into MSIR.
///
/// This is the main entry point for the IR lowering pipeline.
/// Only a subset of the language is supported in the initial pass;
/// unsupported constructs return [`IrError::UnsupportedExpr`].
///
/// # Errors
///
/// Returns an [`IrError`] if lowering fails due to unsupported constructs,
/// unresolved type variables, or unknown effects.
pub fn lower(
    parsed: &ParsedModule,
    sema: &SemaResult,
    interner: &Interner,
) -> Result<IrModule, IrError> {
    decl::lower_module(parsed, sema, interner)
}
