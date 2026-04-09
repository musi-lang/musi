mod api;
mod diag;
mod emit;

pub use api::{
    EmitDiagList, EmitOptions, EmittedBinding, EmittedModule, EmittedProgram, emit_diag_kind,
};
pub use diag::EmitDiagKind;
pub use emit::{lower_ir_module, lower_ir_program};

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
