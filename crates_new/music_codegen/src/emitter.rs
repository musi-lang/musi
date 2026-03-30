use music_basic::SourceMap;
use music_check::AnalyzedModule;
use music_names::Interner;

use crate::errors::EmitResult;
use crate::model::{EmitModule, EmitProgram, ProgramArtifact};

mod function;
mod method;
mod number;
mod program;
mod types;

pub fn emit_single_module(
    path: &str,
    interner: &Interner,
    sources: &SourceMap,
    analyzed: &AnalyzedModule,
) -> EmitResult<ProgramArtifact> {
    emit_program(EmitProgram {
        interner,
        sources,
        modules_in_order: Box::new([EmitModule { path, analyzed }]),
        entry_path: path,
    })
}

pub fn emit_program(program: EmitProgram<'_>) -> EmitResult<ProgramArtifact> {
    let mut emitter =
        program::ProgramEmitter::new(program.interner, program.sources, program.entry_path);
    for module in program.modules_in_order.iter() {
        emitter.register_module(module.path, module.analyzed);
    }
    let artifact = emitter.emit_all(program.modules_in_order.as_ref())?;
    Ok(ProgramArtifact { artifact })
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
