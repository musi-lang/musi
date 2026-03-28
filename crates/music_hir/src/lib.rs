pub mod attrs;
mod bundle;
mod lower;
mod pipeline;

pub use bundle::{TypedModule, TypedProject, type_module, type_project};
pub use lower::lower;
pub use pipeline::{
    CompileResult, ProjectCheckResult, ProjectModuleCheck, analyze_project, compile,
    compile_project, emit_project_diagnostics,
};
