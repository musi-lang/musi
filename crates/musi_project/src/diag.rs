use music_base::diag::{DiagCode, DiagLevel, DiagnosticKind};

#[path = "diag_catalog_gen.rs"]
mod diag_catalog_gen;

pub use diag_catalog_gen::project_error_kind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ProjectDiagKind {
    MissingManifest,
    MissingManifestAncestor,
    MissingPackageRoot,
    MissingModule,
    InvalidManifestJson,
    ProjectIoFailed,
    ManifestValidationFailed,
    DuplicateWorkspaceMember,
    DuplicatePackageName,
    MissingPackageVersion,
    NoRootPackage,
    MissingPackageEntryModule,
    UnknownTask,
    TaskDependencyCycle,
    PackageDependencyCycle,
    UnresolvedImport,
    MissingRegistryRoot,
    MissingGlobalCacheRoot,
    RegistryVersionNotFound,
    InvalidVersionRequirement,
    MissingFrozenLockfile,
    FrozenLockfileOutOfDate,
    UnknownPackage,
    PackageGraphEntryMissing,
    InvalidGitDependency,
    GitCommandFailed,
    GitReferenceNotFound,
    SessionCompilationFailed,
}

impl ProjectDiagKind {
    #[must_use]
    pub fn code(self) -> DiagCode {
        DiagCode::new(diag_catalog_gen::code(self))
    }

    #[must_use]
    pub fn message(self) -> &'static str {
        diag_catalog_gen::message(self)
    }

    #[must_use]
    pub fn label(self) -> &'static str {
        diag_catalog_gen::primary(self)
    }

    #[must_use]
    pub fn hint(self) -> Option<&'static str> {
        diag_catalog_gen::help(self)
    }

    #[must_use]
    pub fn from_code(code: DiagCode) -> Option<Self> {
        diag_catalog_gen::from_code(code.raw())
    }
}

impl DiagnosticKind for ProjectDiagKind {
    fn code(self) -> DiagCode {
        self.code()
    }
    fn phase(self) -> &'static str {
        "project"
    }
    fn level(self) -> DiagLevel {
        DiagLevel::Error
    }
    fn message(self) -> &'static str {
        self.message()
    }
    fn primary(self) -> &'static str {
        self.label()
    }
    fn help(self) -> Option<&'static str> {
        self.hint()
    }
}
