use music_base::diag::{DiagCode, DiagLevel, DiagnosticKind};

#[path = "diag_catalog_gen.rs"]
#[rustfmt::skip]
mod diag_catalog_gen;

pub use diag_catalog_gen::cli_error_kind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CliDiagKind {
    MissingCurrentDirectory,
    TaskFailed,
    UnsupportedRunArgs,
    PackageAlreadyInitialized,
    MissingPackageName,
    UnknownTarget,
    CheckCommandFailed,
    CommandUnavailable,
    LspServerFailed,
    IncompatibleCommandArgs,
}

impl CliDiagKind {
    #[must_use]
    pub fn code(self) -> DiagCode {
        let code = DiagCode::new(diag_catalog_gen::code(self));
        debug_assert_eq!(Self::from_code(code), Some(self));
        code
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

impl DiagnosticKind for CliDiagKind {
    fn code(self) -> DiagCode {
        self.code()
    }
    fn phase(self) -> &'static str {
        "cli"
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
