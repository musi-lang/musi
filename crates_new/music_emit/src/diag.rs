use music_base::diag::{Diag, DiagCode};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EmitDiagKind {
    ExportTargetMissing,
    UnknownTypeValue(Box<str>),
    UnknownTypeNameForOp {
        ty_name: Box<str>,
        op_text: Box<str>,
    },
    UnsupportedBinaryOperator(Box<str>),
    CaseVariantDispatchRequiresSingleDataType,
    UnknownDataType(Box<str>),
    SpreadCallArgsNotEmitted,
    UnknownClosureTarget(Box<str>),
    UnknownEffect,
    UnknownHandlerType(Box<str>),
    UnknownRecordType(Box<str>),
    RecordLiteralMissingFieldValue,
    RecordUpdateMissingFieldValue,
    UnknownSequenceType(Box<str>),
    InvalidIntegerLiteral(Box<str>),
    InvalidFloatLiteral(Box<str>),
    UnsupportedNameRef(Box<str>),
    UnsupportedAssignTarget(Box<str>),
}

impl EmitDiagKind {
    #[must_use]
    pub const fn code(&self) -> DiagCode {
        DiagCode::new(match self {
            Self::ExportTargetMissing => 3500,
            Self::UnknownTypeValue(_) => 3501,
            Self::UnknownTypeNameForOp { .. } => 3502,
            Self::UnsupportedBinaryOperator(_) => 3503,
            Self::CaseVariantDispatchRequiresSingleDataType => 3504,
            Self::UnknownDataType(_) => 3505,
            Self::SpreadCallArgsNotEmitted => 3506,
            Self::UnknownClosureTarget(_) => 3507,
            Self::UnknownEffect => 3508,
            Self::UnknownHandlerType(_) => 3509,
            Self::UnknownRecordType(_) => 3510,
            Self::RecordLiteralMissingFieldValue => 3511,
            Self::RecordUpdateMissingFieldValue => 3512,
            Self::UnknownSequenceType(_) => 3513,
            Self::InvalidIntegerLiteral(_) => 3514,
            Self::InvalidFloatLiteral(_) => 3515,
            Self::UnsupportedNameRef(_) => 3516,
            Self::UnsupportedAssignTarget(_) => 3517,
        })
    }

    #[must_use]
    pub fn message(&self) -> String {
        match self {
            Self::ExportTargetMissing => "export target missing".into(),
            Self::UnknownTypeValue(name) => format!("unknown emitted type value `{name}`"),
            Self::UnknownTypeNameForOp { ty_name, op_text } => {
                format!("unknown type name `{ty_name}` for `{op_text}`")
            }
            Self::UnsupportedBinaryOperator(name) => {
                format!("binary operator `{name}` has no emitted form")
            }
            Self::CaseVariantDispatchRequiresSingleDataType => {
                "case variant dispatch requires single data type".into()
            }
            Self::UnknownDataType(name) => format!("unknown emitted data type `{name}`"),
            Self::SpreadCallArgsNotEmitted => "spread call args have no emitted form".into(),
            Self::UnknownClosureTarget(name) => format!("unknown emitted closure target `{name}`"),
            Self::UnknownEffect => "unknown emitted effect".into(),
            Self::UnknownHandlerType(name) => format!("unknown emitted handler type `{name}`"),
            Self::UnknownRecordType(name) => format!("unknown emitted record type `{name}`"),
            Self::RecordLiteralMissingFieldValue => "record literal missing field value".into(),
            Self::RecordUpdateMissingFieldValue => "record update missing field value".into(),
            Self::UnknownSequenceType(name) => format!("unknown emitted sequence type `{name}`"),
            Self::InvalidIntegerLiteral(raw) => format!("invalid integer literal `{raw}`"),
            Self::InvalidFloatLiteral(raw) => format!("invalid float literal `{raw}`"),
            Self::UnsupportedNameRef(name) => format!("name ref `{name}` has no emitted form"),
            Self::UnsupportedAssignTarget(name) => {
                format!("assignment target `{name}` has no emitted form")
            }
        }
    }

    #[must_use]
    pub fn label(&self) -> String {
        self.message()
    }

    #[must_use]
    pub fn from_diag(diag: &Diag) -> Option<Self> {
        match diag.code()?.raw() {
            3500 => Some(Self::ExportTargetMissing),
            3504 => Some(Self::CaseVariantDispatchRequiresSingleDataType),
            3508 => Some(Self::UnknownEffect),
            3511 => Some(Self::RecordLiteralMissingFieldValue),
            3512 => Some(Self::RecordUpdateMissingFieldValue),
            _ => None,
        }
    }
}
