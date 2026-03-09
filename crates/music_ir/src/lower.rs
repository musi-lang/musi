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
