use music_base::diag::{DiagCode, DiagLevel, DiagnosticKind};

#[path = "diag_catalog_gen.rs"]
mod diag_catalog_gen;

pub use diag_catalog_gen::{artifact_error_kind, assembly_error_kind, hil_verify_error_kind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SeamDiagKind {
    ArtifactValidationFailed,
    InvalidBinaryHeader,
    UnsupportedBinaryVersion,
    UnknownSectionTag,
    BinaryPayloadTruncated,
    UnknownOpcode,
    TextParseFailed,
    InvalidReference,
    DuplicateLabel,
    MissingLabel,
    InvalidEffectOp,
    OperandShapeMismatch,
    HilMissingEntryBlock,
    HilDuplicateBlock,
    HilDuplicateValue,
    HilUndefinedValue,
    HilTypeMismatch,
    HilMissingBlockTarget,
    HilReturnTypeMismatch,
    HilReturnValueMissing,
    HilReturnValueUnexpected,
    HilCapabilityRequired,
}

impl SeamDiagKind {
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

impl DiagnosticKind for SeamDiagKind {
    fn code(self) -> DiagCode {
        self.code()
    }
    fn phase(self) -> &'static str {
        "seam"
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
