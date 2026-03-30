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

    #[error("splice outside quote")]
    SpliceOutsideQuote,

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
    pub(crate) fn diag_code(&self) -> DiagCode {
        match self {
            SemaErrorKind::TypeMismatch { .. } => DiagCode::new(3006),
            SemaErrorKind::MissingWithClause => DiagCode::new(3007),
            SemaErrorKind::InvalidPerformTarget => DiagCode::new(3008),
            SemaErrorKind::UnknownEffect { .. } => DiagCode::new(3009),
            SemaErrorKind::UnknownEffectOp { .. } => DiagCode::new(3010),
            SemaErrorKind::HandleValueClauseRequired => DiagCode::new(3011),
            SemaErrorKind::ResumeOutsideOpClause => DiagCode::new(3012),
            SemaErrorKind::EffectNotDeclared { .. } => DiagCode::new(3013),
            SemaErrorKind::EffectRemainderNotDeclared => DiagCode::new(3014),
            SemaErrorKind::EffectTypeParamCountUnsupported { .. } => DiagCode::new(3015),
            SemaErrorKind::UnknownClass { .. } => DiagCode::new(3016),
            SemaErrorKind::UnknownClassOp { .. } => DiagCode::new(3017),
            SemaErrorKind::InstanceMissingOp { .. } => DiagCode::new(3018),
            SemaErrorKind::InstanceMemberValueRequired => DiagCode::new(3019),
            SemaErrorKind::InvalidInstanceTarget => DiagCode::new(3020),
            SemaErrorKind::ClassTypeParamCountUnsupported { .. } => DiagCode::new(3021),
            SemaErrorKind::SpliceOutsideQuote => DiagCode::new(3022),
            SemaErrorKind::OptionLangItemRequired => DiagCode::new(3023),
            SemaErrorKind::OptionalChainRequiresOption => DiagCode::new(3024),
            SemaErrorKind::ForcedChainRequiresOption => DiagCode::new(3025),
            SemaErrorKind::UnknownLangItem { .. } => DiagCode::new(3026),
            SemaErrorKind::DuplicateLangItem { .. } => DiagCode::new(3027),
            SemaErrorKind::LangItemNameRequired => DiagCode::new(3028),
            SemaErrorKind::LangItemNameRequiresString => DiagCode::new(3029),
            SemaErrorKind::LangItemMissingValue => DiagCode::new(3030),
            SemaErrorKind::LangItemRequiresData => DiagCode::new(3031),
            SemaErrorKind::LangItemRequiresName => DiagCode::new(3032),
            SemaErrorKind::OptionLangItemTypeParamCountUnsupported { .. } => DiagCode::new(3033),
            SemaErrorKind::OptionLangItemFieldsNotAllowed => DiagCode::new(3034),
            SemaErrorKind::OptionLangItemVariantsRequired => DiagCode::new(3035),
            SemaErrorKind::OptionLangItemVariantCountInvalid => DiagCode::new(3036),
            SemaErrorKind::OptionLangItemSomeRequired => DiagCode::new(3037),
            SemaErrorKind::OptionLangItemNoneRequired => DiagCode::new(3038),
            SemaErrorKind::OptionLangItemNoneMustBeNullary => DiagCode::new(3039),
            SemaErrorKind::OptionLangItemSomePayloadInvalid => DiagCode::new(3040),
            SemaErrorKind::AttrUnknownArg { .. } => DiagCode::new(3041),
            SemaErrorKind::AttrDuplicateArg { .. } => DiagCode::new(3042),
            SemaErrorKind::AttrArgCountInvalid { .. } => DiagCode::new(3043),
            SemaErrorKind::AttrNamedArgsNotAllowed { .. } => DiagCode::new(3044),
            SemaErrorKind::AttrArgsRequired { .. } => DiagCode::new(3045),
            SemaErrorKind::LangItemNameDuplicate => DiagCode::new(3046),
            SemaErrorKind::TupleIndexOutOfRange { .. } => DiagCode::new(3047),
            SemaErrorKind::FieldNotFound { .. } => DiagCode::new(3048),
            SemaErrorKind::IndexExceedsArrayNesting => DiagCode::new(3049),
            SemaErrorKind::AssignTargetRequiresMutableBinding { .. } => DiagCode::new(3050),
            SemaErrorKind::AssignTargetRequiresWritableBase => DiagCode::new(3051),
            SemaErrorKind::AssignTargetInvalid => DiagCode::new(3052),
            SemaErrorKind::TuplePatternRequiresTuple => DiagCode::new(3053),
            SemaErrorKind::TuplePatternArityMismatch { .. } => DiagCode::new(3054),
            SemaErrorKind::ArrayPatternRequiresArray => DiagCode::new(3055),
            SemaErrorKind::RecordPatternRequiresRecord => DiagCode::new(3056),
            SemaErrorKind::VariantPatternRequiresData => DiagCode::new(3057),
            SemaErrorKind::VariantNotFound { .. } => DiagCode::new(3058),
            SemaErrorKind::VariantPatternArgCountMismatch { .. } => DiagCode::new(3059),
            SemaErrorKind::PerformArgCountMismatch { .. } => DiagCode::new(3060),
            SemaErrorKind::HandleClauseMissingOp { .. } => DiagCode::new(3061),
            SemaErrorKind::HandleClauseDuplicateOp { .. } => DiagCode::new(3062),
            SemaErrorKind::HandleClauseParamCountMismatch { .. } => DiagCode::new(3063),
            SemaErrorKind::OpaqueTypeBlocksRepresentation { .. } => DiagCode::new(3064),
            SemaErrorKind::ArrayDimOutOfRange => DiagCode::new(3065),
            SemaErrorKind::AttrArgStringRequired { .. } => DiagCode::new(3066),
            SemaErrorKind::AttrArgIntRequired { .. } => DiagCode::new(3067),
            SemaErrorKind::AttrArgRequired { .. } => DiagCode::new(3068),
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
