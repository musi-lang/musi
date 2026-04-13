use music_base::diag::{Diag, DiagCode};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EmitDiagKind {
    ExportTargetMissing,
    UnknownTypeValue,
    UnknownTypeNameForOp,
    UnsupportedBinaryOperator,
    CaseVariantDispatchRequiresSingleDataType,
    UnknownDataType,
    SpreadCallArgsNotEmitted,
    UnknownClosureTarget,
    UnknownEffect,
    UnknownHandlerType,
    UnknownRecordType,
    RecordLiteralMissingFieldValue,
    RecordUpdateMissingFieldValue,
    UnknownSequenceType,
    InvalidSyntaxLiteral,
    InvalidIntegerLiteral,
    InvalidFloatLiteral,
    UnsupportedNameRef,
    UnsupportedAssignTarget,
    EmitInvariantViolated,
}

impl EmitDiagKind {
    const INFO: [(Self, u16, &'static str); 20] = [
        (Self::ExportTargetMissing, 3500, "export target missing"),
        (Self::UnknownTypeValue, 3501, "unknown emitted type value"),
        (Self::UnknownTypeNameForOp, 3502, "unknown type name for op"),
        (
            Self::UnsupportedBinaryOperator,
            3503,
            "binary operator has no emitted form",
        ),
        (
            Self::CaseVariantDispatchRequiresSingleDataType,
            3504,
            "case variant dispatch requires single data type",
        ),
        (Self::UnknownDataType, 3505, "unknown emitted data type"),
        (
            Self::SpreadCallArgsNotEmitted,
            3506,
            "spread call args have no emitted form",
        ),
        (
            Self::UnknownClosureTarget,
            3507,
            "unknown emitted closure target",
        ),
        (Self::UnknownEffect, 3508, "unknown emitted effect"),
        (
            Self::UnknownHandlerType,
            3509,
            "unknown emitted handler type",
        ),
        (Self::UnknownRecordType, 3510, "unknown emitted record type"),
        (
            Self::RecordLiteralMissingFieldValue,
            3511,
            "record literal missing field value",
        ),
        (
            Self::RecordUpdateMissingFieldValue,
            3512,
            "record update missing field value",
        ),
        (
            Self::UnknownSequenceType,
            3513,
            "unknown emitted sequence type",
        ),
        (Self::InvalidSyntaxLiteral, 3514, "invalid syntax literal"),
        (Self::InvalidIntegerLiteral, 3515, "invalid integer literal"),
        (Self::InvalidFloatLiteral, 3516, "invalid float literal"),
        (
            Self::UnsupportedNameRef,
            3517,
            "name ref has no emitted form",
        ),
        (
            Self::UnsupportedAssignTarget,
            3518,
            "assignment target has no emitted form",
        ),
        (Self::EmitInvariantViolated, 3519, "emit invariant violated"),
    ];

    fn info(self) -> (u16, &'static str) {
        Self::INFO
            .iter()
            .find_map(|(kind, code, message)| {
                if *kind == self {
                    Some((*code, *message))
                } else {
                    None
                }
            })
            .expect("missing emit diagnostic info")
    }

    #[must_use]
    pub fn code(&self) -> DiagCode {
        DiagCode::new(self.info().0)
    }

    #[must_use]
    pub fn message(&self) -> &'static str {
        self.info().1
    }

    #[must_use]
    pub fn label(&self) -> &'static str {
        self.message()
    }

    #[must_use]
    pub fn from_code(code: DiagCode) -> Option<Self> {
        Self::INFO.iter().find_map(|(kind, raw, _)| {
            if *raw == code.raw() {
                Some(*kind)
            } else {
                None
            }
        })
    }

    #[must_use]
    pub fn from_diag(diag: &Diag) -> Option<Self> {
        Self::from_code(diag.code()?)
    }
}
