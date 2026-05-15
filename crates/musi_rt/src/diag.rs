use music_base::diag::{DiagCode, DiagLevel, DiagnosticKind};

#[path = "diag_catalog_gen.rs"]
#[rustfmt::skip]
mod diag_catalog_gen;

pub use diag_catalog_gen::runtime_error_kind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeDiagKind {
    RootModuleRequired,
    MissingModuleSource,
    InvalidSyntaxValue,
    SessionFailed,
    VmExecutionFailed,
}

impl RuntimeDiagKind {
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

impl DiagnosticKind for RuntimeDiagKind {
    fn code(self) -> DiagCode {
        self.code()
    }
    fn phase(self) -> &'static str {
        "runtime"
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
