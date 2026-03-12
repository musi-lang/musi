//! `music_emit` — AST+sema → `.msbc` bytecode emitter.
//!
//! The entry point is [`emit`], which takes a parsed module, sema result,
//! and interner, then returns the complete binary contents of a `.msbc` file.
//!
//! # Module layout
//!
//! - [`error`]      — [`EmitError`] enum
//! - [`const_pool`] — constant pool builder
//! - [`type_pool`]  — type pool builder (sema `TypeIdx` → `type_id`)
//! - [`module`]     — `.msbc` binary assembler (header + 5 sections)
//! - [`emitter`]    — orchestrator; tree-walks AST+sema per function

mod const_pool;
mod emitter;
mod module;
mod type_pool;

pub mod error;

pub use error::EmitError;

use music_ast::ParsedModule;
use music_sema::SemaResult;
use music_shared::Interner;

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
) -> Result<EmitOutput, EmitError> {
    let mut emitter = emitter::Emitter::new(parsed, sema, interner);
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
