use music_basic::{Diag, DiagCode, SourceId, Span};
use thiserror::Error;

pub type SemaErrors = Vec<SemaError>;
pub type SemaErrorKinds = Vec<SemaErrorKind>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemaError {
    pub kind: SemaErrorKind,
    pub source_id: SourceId,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum SemaErrorKind {
    #[error("expected type '{expected}', found '{found}'")]
    TypeMismatch { expected: String, found: String },

    #[error("opaque type '{name}' blocks representation access")]
    OpaqueTypeBlocksRepresentation { name: String },

    #[error("array dimension out of range")]
    ArrayDimOutOfRange,

    #[error("assignment target '{name}' requires 'let mut'")]
    AssignTargetRequiresMutableBinding { name: String },

    #[error("assignment target requires writable base")]
    AssignTargetRequiresWritableBase,

    #[error("invalid assignment target")]
    AssignTargetInvalid,

    #[error("tuple pattern requires tuple")]
    TuplePatternRequiresTuple,

    #[error("tuple pattern expects {expected} element(s), found {found}")]
    TuplePatternArityMismatch { expected: u32, found: u32 },

    #[error("array pattern requires array")]
    ArrayPatternRequiresArray,

    #[error("record pattern requires record")]
    RecordPatternRequiresRecord,

    #[error("variant pattern requires data")]
    VariantPatternRequiresData,

    #[error("variant '{name}' not found")]
    VariantNotFound { name: String },

    #[error("variant '{variant}' pattern expects {expected} argument(s), found {found}")]
    VariantPatternArgCountMismatch {
        variant: String,
        expected: String,
        found: u32,
    },

    #[error("perform '{effect}.{op}' expects {expected} argument(s), found {found}")]
    PerformArgCountMismatch {
        effect: String,
        op: String,
        expected: u32,
        found: u32,
    },

    #[error("handler missing operation clause '{effect}.{op}'")]
    HandleClauseMissingOp { effect: String, op: String },

    #[error("handler has duplicate operation clause '{effect}.{op}'")]
    HandleClauseDuplicateOp { effect: String, op: String },

    #[error("handler clause '{effect}.{op}' expects {expected} parameter(s), found {found}")]
    HandleClauseParamCountMismatch {
        effect: String,
        op: String,
        expected: u32,
        found: u32,
    },

    #[error("tuple index {index} out of range for tuple length {len}")]
    TupleIndexOutOfRange { index: u32, len: u32 },

    #[error("field '{name}' not found")]
    FieldNotFound { name: String },

    #[error("index exceeds array nesting")]
    IndexExceedsArrayNesting,

    #[error("effectful expression requires explicit 'with {{ ... }}' signature")]
    MissingWithClause,

    #[error("invalid perform target")]
    InvalidPerformTarget,

    #[error("unknown effect '{name}'")]
    UnknownEffect { name: String },

    #[error("unknown effect operation '{op}' for effect '{effect}'")]
    UnknownEffectOp { effect: String, op: String },

    #[error("handle requires exactly one value clause")]
    HandleValueClauseRequired,

    #[error("resume may only appear inside operation clause body")]
    ResumeOutsideOpClause,

    #[error("effect '{name}' not declared in this signature's effect row")]
    EffectNotDeclared { name: String },

    #[error("effect row remainder not declared in this signature's effect row")]
    EffectRemainderNotDeclared,

    #[error("effect type parameter count {count} unsupported")]
    EffectTypeParamCountUnsupported { count: u32 },

    #[error("unknown class '{name}'")]
    UnknownClass { name: String },

    #[error("unknown class operation '{op}' for class '{class}'")]
    UnknownClassOp { class: String, op: String },

    #[error("instance missing operation '{op}' for class '{class}'")]
    InstanceMissingOp { class: String, op: String },

    #[error("instance member value required")]
    InstanceMemberValueRequired,

    #[error("invalid instance target")]
    InvalidInstanceTarget,

    #[error("class type parameter count {count} unsupported")]
    ClassTypeParamCountUnsupported { count: u32 },

    #[error("'Option' lang item required")]
    OptionLangItemRequired,

    #[error("optional chain requires 'Option[T]'")]
    OptionalChainRequiresOption,

    #[error("forced chain requires 'Option[T]'")]
    ForcedChainRequiresOption,

    #[error("unknown language item item '{name}'")]
    UnknownLangItem { name: String },

    #[error("duplicate language item '{name}'")]
    DuplicateLangItem { name: String },

    #[error("language item requires 'name' argument")]
    LangItemNameRequired,

    #[error("'name' argument requires string literal")]
    LangItemNameRequiresString,

    #[error("language item requires value")]
    LangItemMissingValue,

    #[error("language item requires data definition")]
    LangItemRequiresData,

    #[error("language item requires name binding")]
    LangItemRequiresName,

    #[error("'Option' type parameter count {count} unsupported")]
    OptionLangItemTypeParamCountUnsupported { count: u32 },

    #[error("'Option' fields not allowed")]
    OptionLangItemFieldsNotAllowed,

    #[error("'Option' variants required")]
    OptionLangItemVariantsRequired,

    #[error("'Option' variant count invalid")]
    OptionLangItemVariantCountInvalid,

    #[error("'Option' requires 'Some' variant")]
    OptionLangItemSomeRequired,

    #[error("'Option' requires 'None' variant")]
    OptionLangItemNoneRequired,

    #[error("'None' must be nullary")]
    OptionLangItemNoneMustBeNullary,

    #[error("'Some' payload must be type parameter")]
    OptionLangItemSomePayloadInvalid,

    #[error("unknown attribute argument '{name}' for '{attr}'")]
    AttrUnknownArg { attr: String, name: String },

    #[error("duplicate attribute argument '{name}' for '{attr}'")]
    AttrDuplicateArg { attr: String, name: String },

    #[error("attribute argument '{name}' for '{attr}' requires string literal")]
    AttrArgStringRequired { attr: String, name: String },

    #[error("attribute argument '{name}' for '{attr}' requires integer literal")]
    AttrArgIntRequired { attr: String, name: String },

    #[error("attribute argument '{name}' for '{attr}' required")]
    AttrArgRequired { attr: String, name: String },

    #[error("expected {expected} attribute argument(s) for '{attr}', found {found}")]
    AttrArgCountInvalid {
        attr: String,
        expected: u32,
        found: u32,
    },

    #[error("named attribute arguments not allowed for '{attr}'")]
    AttrNamedArgsNotAllowed { attr: String },

    #[error("attribute arguments required for '{attr}'")]
    AttrArgsRequired { attr: String },

    #[error("language item 'name' argument duplicated")]
    LangItemNameDuplicate,
}

impl SemaError {
    #[must_use]
    pub fn to_diag(&self) -> Diag {
        let mut diag = Diag::error(self.kind.to_string())
            .with_code(self.kind.diag_code())
            .with_label(self.span, self.source_id, "");

        match &self.kind {
            SemaErrorKind::MissingWithClause => {
                diag = diag.with_hint("add 'with { ... }' clause to declaration signature");
            }
            SemaErrorKind::EffectRemainderNotDeclared => {
                diag = diag
                    .with_hint("make signature effect row open by adding remainder like '...r'");
            }
            SemaErrorKind::OptionLangItemRequired => {
                diag = diag.with_hint("add @musi.lang(name := \"Option\") Option definition");
            }
            SemaErrorKind::LangItemNameRequired => {
                diag = diag.with_hint("use @musi.lang(name := \"...\") form");
            }
            _ => {}
        }

        diag
    }
}

impl SemaErrorKind {
    #[must_use]
    const fn diag_code(&self) -> DiagCode {
        match self {
            Self::TypeMismatch { .. } => DiagCode::new(3006),
            Self::MissingWithClause => DiagCode::new(3007),
            Self::InvalidPerformTarget => DiagCode::new(3008),
            Self::UnknownEffect { .. } => DiagCode::new(3009),
            Self::UnknownEffectOp { .. } => DiagCode::new(3010),
            Self::HandleValueClauseRequired => DiagCode::new(3011),
            Self::ResumeOutsideOpClause => DiagCode::new(3012),
            Self::EffectNotDeclared { .. } => DiagCode::new(3013),
            Self::EffectRemainderNotDeclared => DiagCode::new(3014),
            Self::EffectTypeParamCountUnsupported { .. } => DiagCode::new(3015),
            Self::UnknownClass { .. } => DiagCode::new(3016),
            Self::UnknownClassOp { .. } => DiagCode::new(3017),
            Self::InstanceMissingOp { .. } => DiagCode::new(3018),
            Self::InstanceMemberValueRequired => DiagCode::new(3019),
            Self::InvalidInstanceTarget => DiagCode::new(3020),
            Self::ClassTypeParamCountUnsupported { .. } => DiagCode::new(3021),
            Self::OptionLangItemRequired => DiagCode::new(3023),
            Self::OptionalChainRequiresOption => DiagCode::new(3024),
            Self::ForcedChainRequiresOption => DiagCode::new(3025),
            Self::UnknownLangItem { .. } => DiagCode::new(3026),
            Self::DuplicateLangItem { .. } => DiagCode::new(3027),
            Self::LangItemNameRequired => DiagCode::new(3028),
            Self::LangItemNameRequiresString => DiagCode::new(3029),
            Self::LangItemMissingValue => DiagCode::new(3030),
            Self::LangItemRequiresData => DiagCode::new(3031),
            Self::LangItemRequiresName => DiagCode::new(3032),
            Self::OptionLangItemTypeParamCountUnsupported { .. } => DiagCode::new(3033),
            Self::OptionLangItemFieldsNotAllowed => DiagCode::new(3034),
            Self::OptionLangItemVariantsRequired => DiagCode::new(3035),
            Self::OptionLangItemVariantCountInvalid => DiagCode::new(3036),
            Self::OptionLangItemSomeRequired => DiagCode::new(3037),
            Self::OptionLangItemNoneRequired => DiagCode::new(3038),
            Self::OptionLangItemNoneMustBeNullary => DiagCode::new(3039),
            Self::OptionLangItemSomePayloadInvalid => DiagCode::new(3040),
            Self::AttrUnknownArg { .. } => DiagCode::new(3041),
            Self::AttrDuplicateArg { .. } => DiagCode::new(3042),
            Self::AttrArgCountInvalid { .. } => DiagCode::new(3043),
            Self::AttrNamedArgsNotAllowed { .. } => DiagCode::new(3044),
            Self::AttrArgsRequired { .. } => DiagCode::new(3045),
            Self::LangItemNameDuplicate => DiagCode::new(3046),
            Self::TupleIndexOutOfRange { .. } => DiagCode::new(3047),
            Self::FieldNotFound { .. } => DiagCode::new(3048),
            Self::IndexExceedsArrayNesting => DiagCode::new(3049),
            Self::AssignTargetRequiresMutableBinding { .. } => DiagCode::new(3050),
            Self::AssignTargetRequiresWritableBase => DiagCode::new(3051),
            Self::AssignTargetInvalid => DiagCode::new(3052),
            Self::TuplePatternRequiresTuple => DiagCode::new(3053),
            Self::TuplePatternArityMismatch { .. } => DiagCode::new(3054),
            Self::ArrayPatternRequiresArray => DiagCode::new(3055),
            Self::RecordPatternRequiresRecord => DiagCode::new(3056),
            Self::VariantPatternRequiresData => DiagCode::new(3057),
            Self::VariantNotFound { .. } => DiagCode::new(3058),
            Self::VariantPatternArgCountMismatch { .. } => DiagCode::new(3059),
            Self::PerformArgCountMismatch { .. } => DiagCode::new(3060),
            Self::HandleClauseMissingOp { .. } => DiagCode::new(3061),
            Self::HandleClauseDuplicateOp { .. } => DiagCode::new(3062),
            Self::HandleClauseParamCountMismatch { .. } => DiagCode::new(3063),
            Self::OpaqueTypeBlocksRepresentation { .. } => DiagCode::new(3064),
            Self::ArrayDimOutOfRange => DiagCode::new(3065),
            Self::AttrArgStringRequired { .. } => DiagCode::new(3066),
            Self::AttrArgIntRequired { .. } => DiagCode::new(3067),
            Self::AttrArgRequired { .. } => DiagCode::new(3068),
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
