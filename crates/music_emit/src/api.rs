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

impl EmittedBinding {
    #[must_use]
    pub fn new(
        name: impl Into<Box<str>>,
        method: Option<MethodId>,
        global: Option<GlobalId>,
    ) -> Self {
        Self {
            name: name.into(),
            method,
            global,
        }
    }

    #[must_use]
    pub fn method(name: impl Into<Box<str>>, method: MethodId) -> Self {
        Self::new(name, Some(method), None)
    }

    #[must_use]
    pub fn global(name: impl Into<Box<str>>, global: GlobalId) -> Self {
        Self::new(name, None, Some(global))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmittedModule {
    pub module_key: ModuleKey,
    pub artifact: Artifact,
    pub entry_method: Option<MethodId>,
    pub exports: Box<[EmittedBinding]>,
    pub static_imports: Box<[ModuleKey]>,
}

impl EmittedModule {
    #[must_use]
    pub const fn new(
        module_key: ModuleKey,
        artifact: Artifact,
        entry_method: Option<MethodId>,
        exports: Box<[EmittedBinding]>,
        static_imports: Box<[ModuleKey]>,
    ) -> Self {
        Self {
            module_key,
            artifact,
            entry_method,
            exports,
            static_imports,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmittedProgram {
    pub entry_module: ModuleKey,
    pub artifact: Artifact,
    pub entry_method: MethodId,
    pub modules: Box<[ModuleKey]>,
}

impl EmittedProgram {
    #[must_use]
    pub const fn new(
        entry_module: ModuleKey,
        artifact: Artifact,
        entry_method: MethodId,
        modules: Box<[ModuleKey]>,
    ) -> Self {
        Self {
            entry_module,
            artifact,
            entry_method,
            modules,
        }
    }
}
