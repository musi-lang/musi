use music_base::diag::{Diag, DiagCode, DiagContext, DiagLevel, DiagnosticKind};

#[path = "diag_catalog_gen.rs"]
#[rustfmt::skip]
mod diag_catalog_gen;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IrDiagKind {
    InvalidSurfaceTypeId,
    LoweringRequiresSemaCleanModule,
    LoweringInvariantViolated,
    LoweringInputMissing,
    LoweringStateInvalid,
    LoweringTargetUnsupported,
    LoweringSymbolUnknown,
    LoweringOverflow,
}

impl IrDiagKind {
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
    pub fn message_with(self, context: &DiagContext) -> String {
        diag_catalog_gen::render_message(self, context)
    }

    #[must_use]
    pub fn label_with(self, context: &DiagContext) -> String {
        diag_catalog_gen::render_primary(self, context)
    }

    #[must_use]
    pub fn hint(self) -> Option<&'static str> {
        diag_catalog_gen::help(self)
    }

    #[must_use]
    pub fn from_code(code: DiagCode) -> Option<Self> {
        diag_catalog_gen::from_code(code.raw())
    }

    #[must_use]
    pub fn from_diag(diag: &Diag) -> Option<Self> {
        diag.code().and_then(Self::from_code)
    }
}

impl DiagnosticKind for IrDiagKind {
    fn code(self) -> DiagCode {
        self.code()
    }

    fn phase(self) -> &'static str {
        "ir"
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
