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

    #[error("effect type parameter count '{count}' unsupported")]
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

    #[error("class type parameter count '{count}' unsupported")]
    ClassTypeParamCountUnsupported { count: u32 },

    #[error("splice outside quote")]
    SpliceOutsideQuote,

    #[error("Option lang item required")]
    OptionLangItemRequired,

    #[error("optional chain requires Option[T]")]
    OptionalChainRequiresOption,

    #[error("forced chain requires Option[T]")]
    ForcedChainRequiresOption,

    #[error("unknown lang item '{name}'")]
    UnknownLangItem { name: String },

    #[error("duplicate lang item '{name}'")]
    DuplicateLangItem { name: String },

    #[error("@musi.lang requires 'name' argument")]
    LangItemNameRequired,

    #[error("'name' argument requires string literal")]
    LangItemNameRequiresString,

    #[error("lang item requires value")]
    LangItemMissingValue,

    #[error("lang item requires data definition")]
    LangItemRequiresData,

    #[error("lang item requires name binding")]
    LangItemRequiresName,

    #[error("Option type parameter count '{count}' unsupported")]
    OptionLangItemTypeParamCountUnsupported { count: u32 },

    #[error("Option fields not allowed")]
    OptionLangItemFieldsNotAllowed,

    #[error("Option variants required")]
    OptionLangItemVariantsRequired,

    #[error("Option variant count invalid")]
    OptionLangItemVariantCountInvalid,

    #[error("Option requires 'Some' variant")]
    OptionLangItemSomeRequired,

    #[error("Option requires 'None' variant")]
    OptionLangItemNoneRequired,

    #[error("'None' must be nullary")]
    OptionLangItemNoneMustBeNullary,

    #[error("'Some' payload must be type parameter")]
    OptionLangItemSomePayloadInvalid,
}

impl SemaError {
    #[must_use]
    pub fn to_diag(&self) -> Diag {
        match &self.kind {
            SemaErrorKind::TypeMismatch { .. } => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3006))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::MissingWithClause => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3007))
                .with_label(self.span, self.source_id, "")
                .with_hint("add 'with { ... }' clause to declaration signature"),
            SemaErrorKind::InvalidPerformTarget => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3008))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::UnknownEffect { .. } => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3009))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::UnknownEffectOp { .. } => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3010))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::HandleValueClauseRequired => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3011))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::ResumeOutsideOpClause => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3012))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::EffectNotDeclared { .. } => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3013))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::EffectRemainderNotDeclared => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3014))
                .with_label(self.span, self.source_id, "")
                .with_hint("make signature effect row open by adding remainder like '...r'"),
            SemaErrorKind::EffectTypeParamCountUnsupported { .. } => {
                Diag::error(self.kind.to_string())
                    .with_code(DiagCode::new(3015))
                    .with_label(self.span, self.source_id, "")
            }
            SemaErrorKind::UnknownClass { .. } => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3016))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::UnknownClassOp { .. } => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3017))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::InstanceMissingOp { .. } => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3018))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::InstanceMemberValueRequired => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3019))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::InvalidInstanceTarget => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3020))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::ClassTypeParamCountUnsupported { .. } => {
                Diag::error(self.kind.to_string())
                    .with_code(DiagCode::new(3021))
                    .with_label(self.span, self.source_id, "")
            }
            SemaErrorKind::SpliceOutsideQuote => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3022))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::OptionLangItemRequired => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3023))
                .with_label(self.span, self.source_id, "")
                .with_hint("add @musi.lang(name := \"Option\") Option definition"),
            SemaErrorKind::OptionalChainRequiresOption => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3024))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::ForcedChainRequiresOption => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3025))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::UnknownLangItem { .. } => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3026))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::DuplicateLangItem { .. } => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3027))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::LangItemNameRequired => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3028))
                .with_label(self.span, self.source_id, "")
                .with_hint("use @musi.lang(name := \"...\") form"),
            SemaErrorKind::LangItemNameRequiresString => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3029))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::LangItemMissingValue => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3030))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::LangItemRequiresData => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3031))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::LangItemRequiresName => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3032))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::OptionLangItemTypeParamCountUnsupported { .. } => {
                Diag::error(self.kind.to_string())
                    .with_code(DiagCode::new(3033))
                    .with_label(self.span, self.source_id, "")
            }
            SemaErrorKind::OptionLangItemFieldsNotAllowed => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3034))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::OptionLangItemVariantsRequired => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3035))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::OptionLangItemVariantCountInvalid => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3036))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::OptionLangItemSomeRequired => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3037))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::OptionLangItemNoneRequired => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3038))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::OptionLangItemNoneMustBeNullary => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3039))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::OptionLangItemSomePayloadInvalid => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3040))
                .with_label(self.span, self.source_id, ""),
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
