mod attrs;
mod bundle;
mod lower;
mod pipeline;

pub use attrs::{AttrBindError, AttrSpec, attr_expr_string, attr_path_matches, bind_attr};
pub use bundle::{TypedModule, TypedProject, type_module, type_project};
pub use lower::lower;
pub use pipeline::{
    CompileResult, ProjectCheckResult, ProjectModuleCheck, analyze_project, compile,
    compile_project, emit_project_diagnostics,
};
