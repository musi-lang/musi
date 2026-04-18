use music_base::diag::Diag;
use music_module::ModuleKey;
use music_seam::{Artifact, GlobalId, ProcedureId};

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
    pub procedure: Option<ProcedureId>,
    pub global: Option<GlobalId>,
}

impl EmittedBinding {
    #[must_use]
    pub const fn new(
        name: Box<str>,
        procedure: Option<ProcedureId>,
        global: Option<GlobalId>,
    ) -> Self {
        Self {
            name,
            procedure,
            global,
        }
    }

    #[must_use]
    pub const fn procedure(name: Box<str>, procedure: ProcedureId) -> Self {
        Self::new(name, Some(procedure), None)
    }

    #[must_use]
    pub const fn global(name: Box<str>, global: GlobalId) -> Self {
        Self::new(name, None, Some(global))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmittedModule {
    pub module_key: ModuleKey,
    pub artifact: Artifact,
    pub entry_procedure: Option<ProcedureId>,
    pub exports: Box<[EmittedBinding]>,
    pub static_imports: Box<[ModuleKey]>,
}

impl EmittedModule {
    #[must_use]
    pub const fn new(
        module_key: ModuleKey,
        artifact: Artifact,
        entry_procedure: Option<ProcedureId>,
        exports: Box<[EmittedBinding]>,
        static_imports: Box<[ModuleKey]>,
    ) -> Self {
        Self {
            module_key,
            artifact,
            entry_procedure,
            exports,
            static_imports,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmittedProgram {
    pub entry_module: ModuleKey,
    pub artifact: Artifact,
    pub entry_procedure: ProcedureId,
    pub modules: Box<[ModuleKey]>,
}

impl EmittedProgram {
    #[must_use]
    pub const fn new(
        entry_module: ModuleKey,
        artifact: Artifact,
        entry_procedure: ProcedureId,
        modules: Box<[ModuleKey]>,
    ) -> Self {
        Self {
            entry_module,
            artifact,
            entry_procedure,
            modules,
        }
    }
}
