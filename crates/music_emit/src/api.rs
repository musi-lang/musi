use music_base::diag::Diag;
use music_module::ModuleKey;
use music_seam::{Artifact, GlobalId, MethodId};

use crate::EmitDiagKind;

pub type EmitDiagList = Vec<Diag>;

/// Extract stable emit diagnostic kind from a generic diagnostic.
///
/// This extraction is code-based (`DiagCode`) and does not depend on diagnostic
/// message or label text.
#[must_use]
pub fn emit_diag_kind(diag: &Diag) -> Option<EmitDiagKind> {
    EmitDiagKind::from_diag(diag)
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct EmitOptions;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmittedBinding {
    pub name: Box<str>,
    pub method: Option<MethodId>,
    pub global: Option<GlobalId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmittedModule {
    pub module_key: ModuleKey,
    pub artifact: Artifact,
    pub entry_method: Option<MethodId>,
    pub exports: Box<[EmittedBinding]>,
    pub static_imports: Box<[ModuleKey]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmittedProgram {
    pub entry_module: ModuleKey,
    pub artifact: Artifact,
    pub entry_method: MethodId,
    pub modules: Box<[ModuleKey]>,
}
