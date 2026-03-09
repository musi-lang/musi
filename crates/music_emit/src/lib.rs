//! `music_emit` — MSIR → `.msbc` bytecode emitter.
//!
//! The entry point is [`emit`], which takes an [`IrModule`] and an [`Interner`]
//! and returns the complete binary contents of a `.msbc` file.
//!
//! # Module layout
//!
//! - [`error`]      — [`EmitError`] enum
//! - [`opcode`]     — [`Opcode`] enum (75 opcodes), encoding helpers
//! - [`const_pool`] — constant pool builder
//! - [`type_pool`]  — type pool builder (`Idx<IrType>` → `type_id`)
//! - [`module`]     — `.msbc` binary assembler (header + 4 sections)
//! - [`emitter`]    — orchestrator; calls [`emitter::fn_emitter`] per function

mod const_pool;
mod emitter;
mod module;
mod opcode;
mod type_pool;

pub mod error;

pub use error::EmitError;

use music_ir::IrModule;
use music_shared::Interner;

/// Output of a successful [`emit`] call.
#[derive(Debug)]
pub struct EmitOutput {
    /// Complete `.msbc` file contents (little-endian binary).
    pub bytes: Vec<u8>,
}

/// Emit an [`IrModule`] to `.msbc` bytecode.
///
/// # Errors
///
/// Returns [`EmitError`] if:
/// - Any constant or type pool exceeds its size limit (65535 entries).
/// - A function body exceeds the maximum code length.
/// - A jump offset cannot be encoded as an i16.
/// - A label referenced by a jump cannot be resolved.
/// - An IR type cannot be lowered (e.g. unresolved type parameter).
#[must_use = "check for errors; the caller must handle EmitError"]
pub fn emit(module: &IrModule, interner: &Interner) -> Result<EmitOutput, EmitError> {
    let mut emitter = emitter::Emitter::new(module, interner);
    let functions = emitter.emit_functions()?;

    let entry_fn_id = module.entry.map(|idx| module.functions[idx].id.0);

    let bytes = module::assemble(
        &mut emitter.cp,
        &mut emitter.tp,
        &functions,
        &module.effects,
        &module.types,
        interner,
        entry_fn_id,
    )?;
    Ok(EmitOutput { bytes })
}
