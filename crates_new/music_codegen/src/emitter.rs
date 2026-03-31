use music_basic::SourceMap;
use music_check::AnalyzedModule;
use music_known::KnownSymbols;
use music_names::Interner;

use crate::errors::EmitResult;
use crate::model::{EmitModule, EmitProgram, ProgramArtifact};

use self::program::ProgramEmitter;

mod context;
mod function;
mod method;
mod number;
mod program;
mod ty;

/// Emits SEAM artifact for single analyzed module.
///
/// # Errors
/// Returns `Err` when emission fails.
pub fn emit_single_program(
    path: &str,
    interner: &Interner,
    sources: &SourceMap,
    known: KnownSymbols,
    analyzed: &AnalyzedModule,
) -> EmitResult<ProgramArtifact> {
    let program = EmitProgram {
        interner,
        sources,
        known,
        modules_in_order: Box::new([EmitModule { path, analyzed }]),
        entry_path: path,
    };
    emit_program(&program)
}

/// Emits SEAM artifact for set of analyzed modules.
///
/// # Errors
/// Returns `Err` when emission fails.
pub fn emit_program(program: &EmitProgram<'_>) -> EmitResult<ProgramArtifact> {
    let mut emitter = ProgramEmitter::new(
        program.interner,
        program.sources,
        program.entry_path,
        program.known,
    );
    for module in &program.modules_in_order {
        emitter.register_module(module.path, module.analyzed);
    }
    let artifact = emitter.emit_all(program.modules_in_order.as_ref())?;
    Ok(ProgramArtifact { artifact })
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
