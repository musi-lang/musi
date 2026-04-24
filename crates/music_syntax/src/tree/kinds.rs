#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SyntaxNodeKind {
    Error,
    SourceFile,
    FixityDirective,
    SequenceExpr,

    LetExpr,
    ReceiverMethodHead,
    ImportExpr,
    ForeignBlockExpr,
    DataExpr,
    EffectExpr,
    ShapeExpr,
    GivenExpr,
    NameExpr,
    LiteralExpr,
    TemplateExpr,
    TupleExpr,
    ArrayExpr,
    RecordExpr,
    VariantExpr,
    PiExpr,
    LambdaExpr,
    CallExpr,
    ApplyExpr,
    FieldExpr,
    IndexExpr,
    RecordUpdateExpr,
    TypeTestExpr,
    TypeCastExpr,
    PrefixExpr,
    PostfixExpr,
    InfixExpr,
    BinaryExpr,
    MatchExpr,
    MatchArm,
    AskExpr,
    AnswerLitExpr,
    HandleExpr,
    ResumeExpr,
    QuoteExpr,
    SpliceExpr,
    UnsafeExpr,
    PinExpr,
    AttributedExpr,
    ExportMod,
    WildcardPat,
    BindPat,
    LiteralPat,
    VariantPat,
    RecordPat,
    TuplePat,
    ArrayPat,
    OrPat,
    AsPat,

    NamedTy,
    FunctionTy,
    BinaryTy,
    PiTy,
    TupleTy,
    ArrayTy,
    AnswerTy,

    Attr,
    AttrArg,
    ArrayItem,
    RecordItem,
    VariantPayloadList,
    VariantFieldDef,
    VariantArg,
    VariantPatArg,
    EffectSet,
    EffectItem,
    Arg,
    ParamList,
    Param,
    FieldList,
    Field,
    VariantList,
    Variant,
    TypeParamList,
    TypeParam,
    ConstraintList,
    Constraint,
    HandlerClauseList,
    HandlerClause,
    MemberList,
    Member,
}

impl SyntaxNodeKind {
    #[must_use]
    pub fn is_expr(self) -> bool {
        EXPR_KINDS.contains(&self)
    }

    #[must_use]
    pub fn is_pat(self) -> bool {
        PAT_KINDS.contains(&self)
    }

    #[must_use]
    pub fn is_ty(self) -> bool {
        TY_KINDS.contains(&self)
    }
}

const EXPR_KINDS: &[SyntaxNodeKind] = &[
    SyntaxNodeKind::SequenceExpr,
    SyntaxNodeKind::LetExpr,
    SyntaxNodeKind::ImportExpr,
    SyntaxNodeKind::ForeignBlockExpr,
    SyntaxNodeKind::DataExpr,
    SyntaxNodeKind::EffectExpr,
    SyntaxNodeKind::ShapeExpr,
    SyntaxNodeKind::GivenExpr,
    SyntaxNodeKind::NameExpr,
    SyntaxNodeKind::LiteralExpr,
    SyntaxNodeKind::TemplateExpr,
    SyntaxNodeKind::TupleExpr,
    SyntaxNodeKind::ArrayExpr,
    SyntaxNodeKind::RecordExpr,
    SyntaxNodeKind::VariantExpr,
    SyntaxNodeKind::PiExpr,
    SyntaxNodeKind::LambdaExpr,
    SyntaxNodeKind::CallExpr,
    SyntaxNodeKind::ApplyExpr,
    SyntaxNodeKind::FieldExpr,
    SyntaxNodeKind::IndexExpr,
    SyntaxNodeKind::RecordUpdateExpr,
    SyntaxNodeKind::TypeTestExpr,
    SyntaxNodeKind::TypeCastExpr,
    SyntaxNodeKind::PrefixExpr,
    SyntaxNodeKind::PostfixExpr,
    SyntaxNodeKind::InfixExpr,
    SyntaxNodeKind::BinaryExpr,
    SyntaxNodeKind::MatchExpr,
    SyntaxNodeKind::MatchArm,
    SyntaxNodeKind::AskExpr,
    SyntaxNodeKind::AnswerLitExpr,
    SyntaxNodeKind::HandleExpr,
    SyntaxNodeKind::ResumeExpr,
    SyntaxNodeKind::QuoteExpr,
    SyntaxNodeKind::SpliceExpr,
    SyntaxNodeKind::UnsafeExpr,
    SyntaxNodeKind::PinExpr,
    SyntaxNodeKind::AttributedExpr,
];

const PAT_KINDS: &[SyntaxNodeKind] = &[
    SyntaxNodeKind::WildcardPat,
    SyntaxNodeKind::BindPat,
    SyntaxNodeKind::LiteralPat,
    SyntaxNodeKind::VariantPat,
    SyntaxNodeKind::RecordPat,
    SyntaxNodeKind::TuplePat,
    SyntaxNodeKind::ArrayPat,
    SyntaxNodeKind::OrPat,
    SyntaxNodeKind::AsPat,
];

const TY_KINDS: &[SyntaxNodeKind] = &[
    SyntaxNodeKind::NamedTy,
    SyntaxNodeKind::FunctionTy,
    SyntaxNodeKind::BinaryTy,
    SyntaxNodeKind::PiTy,
    SyntaxNodeKind::TupleTy,
    SyntaxNodeKind::ArrayTy,
    SyntaxNodeKind::AnswerTy,
];

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
