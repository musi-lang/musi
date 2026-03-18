//! `msc_emit` - AST+sema -> `.msbc` bytecode emitter.
//!
//! The entry point is [`emit`], which takes a parsed module, sema result,
//! and interner, then returns the complete binary contents of a `.msbc` file.
//!
//! # Module layout
//!
//! - [`error`]      - [`EmitError`] enum
//! - [`const_pool`] - constant pool builder
//! - [`type_pool`]  - type pool builder (sema `TypeIdx` -> `type_id`)
//! - [`module`]     - `.msbc` binary assembler (header + 5 sections)
//! - [`emitter`]    - orchestrator; tree-walks AST+sema per function

mod const_pool;
mod emitter;
mod module;
mod type_pool;

pub mod error;

pub use error::EmitError;

use std::collections::HashMap;

use msc_ast::{ExprIdx, ParsedModule};
use msc_sema::def::DefId;
use msc_sema::{DictLookup, Obligation, ResolutionMap, SemaResult, TypeIdx};
use msc_shared::{FileId, Interner};

/// Dependency module data needed by the emitter.
pub struct DepEmitInput<'a> {
    pub parsed: &'a ParsedModule,
    pub resolution: &'a ResolutionMap,
    pub expr_types: &'a HashMap<ExprIdx, TypeIdx>,
    pub binop_dispatch: &'a HashMap<ExprIdx, DefId>,
    pub binop_dict_dispatch: &'a HashMap<ExprIdx, DictLookup>,
    pub fn_constraints: &'a HashMap<DefId, Vec<Obligation>>,
}

/// Output of a successful [`emit`] call.
#[derive(Debug)]
pub struct EmitOutput {
    /// Complete `.msbc` file contents (little-endian binary).
    pub bytes: Vec<u8>,
}

/// Emit bytecode from a parsed+typechecked module.
///
/// # Errors
///
/// Returns [`EmitError`] if:
/// - Any constant or type pool exceeds its size limit (65535 entries).
/// - A function body exceeds the maximum code length.
/// - A jump offset cannot be encoded as an i32.
/// - A label referenced by a jump cannot be resolved.
#[must_use = "check for errors; the caller must handle EmitError"]
pub fn emit(
    parsed: &ParsedModule,
    sema: &SemaResult,
    interner: &mut Interner,
    file_id: FileId,
    script: bool,
    deps: &[DepEmitInput<'_>],
) -> Result<EmitOutput, EmitError> {
    let mut emitter = emitter::Emitter::new(parsed, sema, interner, file_id, script, deps);
    let functions = emitter.emit_all()?;

    let bytes = module::assemble(module::AssembleParams {
        cp: &mut emitter.cp,
        tp: &mut emitter.tp,
        functions: &functions,
        effects: &emitter.effects,
        foreign_fns: &emitter.foreign_fns,
        interner: emitter.interner,
        entry_fn_id: emitter.entry_fn_id,
    })?;
    Ok(EmitOutput { bytes })
}
