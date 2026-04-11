use music_base::diag::{Diag, DiagCode};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    #[must_use]
    pub const fn code(&self) -> DiagCode {
        DiagCode::new(match self {
            Self::ExportTargetMissing => 3500,
            Self::UnknownTypeValue => 3501,
            Self::UnknownTypeNameForOp => 3502,
            Self::UnsupportedBinaryOperator => 3503,
            Self::CaseVariantDispatchRequiresSingleDataType => 3504,
            Self::UnknownDataType => 3505,
            Self::SpreadCallArgsNotEmitted => 3506,
            Self::UnknownClosureTarget => 3507,
            Self::UnknownEffect => 3508,
            Self::UnknownHandlerType => 3509,
            Self::UnknownRecordType => 3510,
            Self::RecordLiteralMissingFieldValue => 3511,
            Self::RecordUpdateMissingFieldValue => 3512,
            Self::UnknownSequenceType => 3513,
            Self::InvalidSyntaxLiteral => 3514,
            Self::InvalidIntegerLiteral => 3515,
            Self::InvalidFloatLiteral => 3516,
            Self::UnsupportedNameRef => 3517,
            Self::UnsupportedAssignTarget => 3518,
            Self::EmitInvariantViolated => 3519,
        })
    }

    #[must_use]
    pub const fn message(&self) -> &'static str {
        match self {
            Self::ExportTargetMissing => "export target missing",
            Self::UnknownTypeValue => "unknown emitted type value",
            Self::UnknownTypeNameForOp => "unknown type name for op",
            Self::UnsupportedBinaryOperator => "binary operator has no emitted form",
            Self::CaseVariantDispatchRequiresSingleDataType => {
                "case variant dispatch requires single data type"
            }
            Self::UnknownDataType => "unknown emitted data type",
            Self::SpreadCallArgsNotEmitted => "spread call args have no emitted form",
            Self::UnknownClosureTarget => "unknown emitted closure target",
            Self::UnknownEffect => "unknown emitted effect",
            Self::UnknownHandlerType => "unknown emitted handler type",
            Self::UnknownRecordType => "unknown emitted record type",
            Self::RecordLiteralMissingFieldValue => "record literal missing field value",
            Self::RecordUpdateMissingFieldValue => "record update missing field value",
            Self::UnknownSequenceType => "unknown emitted sequence type",
            Self::InvalidSyntaxLiteral => "invalid syntax literal",
            Self::InvalidIntegerLiteral => "invalid integer literal",
            Self::InvalidFloatLiteral => "invalid float literal",
            Self::UnsupportedNameRef => "name ref has no emitted form",
            Self::UnsupportedAssignTarget => "assignment target has no emitted form",
            Self::EmitInvariantViolated => "emit invariant violated",
        }
    }

    #[must_use]
    pub const fn label(&self) -> &'static str {
        self.message()
    }

    #[must_use]
    pub const fn from_code(code: DiagCode) -> Option<Self> {
        match code.raw() {
            3500 => Some(Self::ExportTargetMissing),
            3501 => Some(Self::UnknownTypeValue),
            3502 => Some(Self::UnknownTypeNameForOp),
            3503 => Some(Self::UnsupportedBinaryOperator),
            3504 => Some(Self::CaseVariantDispatchRequiresSingleDataType),
            3505 => Some(Self::UnknownDataType),
            3506 => Some(Self::SpreadCallArgsNotEmitted),
            3507 => Some(Self::UnknownClosureTarget),
            3508 => Some(Self::UnknownEffect),
            3509 => Some(Self::UnknownHandlerType),
            3510 => Some(Self::UnknownRecordType),
            3511 => Some(Self::RecordLiteralMissingFieldValue),
            3512 => Some(Self::RecordUpdateMissingFieldValue),
            3513 => Some(Self::UnknownSequenceType),
            3514 => Some(Self::InvalidSyntaxLiteral),
            3515 => Some(Self::InvalidIntegerLiteral),
            3516 => Some(Self::InvalidFloatLiteral),
            3517 => Some(Self::UnsupportedNameRef),
            3518 => Some(Self::UnsupportedAssignTarget),
            3519 => Some(Self::EmitInvariantViolated),
            _ => None,
        }
    }

    #[must_use]
    pub fn from_diag(diag: &Diag) -> Option<Self> {
        Self::from_code(diag.code()?)
    }
}
