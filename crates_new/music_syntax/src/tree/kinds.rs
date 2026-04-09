#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SyntaxNodeKind {
    Error,
    SourceFile,
    FixityDirective,
    SequenceExpr,

    LetExpr,
    ImportExpr,
    ForeignBlockExpr,
    DataExpr,
    EffectExpr,
    ClassExpr,
    InstanceExpr,
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
    InfixExpr,
    BinaryExpr,
    CaseExpr,
    CaseArm,
    PerformExpr,
    HandleExpr,
    ResumeExpr,
    QuoteExpr,
    SpliceExpr,
    AttributedExpr,
    ExportExpr,

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

    Attr,
    AttrArg,
    ArrayItem,
    RecordItem,
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
    pub const fn is_expr(self) -> bool {
        matches!(
            self,
            Self::SequenceExpr
                | Self::LetExpr
                | Self::ImportExpr
                | Self::ForeignBlockExpr
                | Self::DataExpr
                | Self::EffectExpr
                | Self::ClassExpr
                | Self::InstanceExpr
                | Self::NameExpr
                | Self::LiteralExpr
                | Self::TemplateExpr
                | Self::TupleExpr
                | Self::ArrayExpr
                | Self::RecordExpr
                | Self::VariantExpr
                | Self::PiExpr
                | Self::LambdaExpr
                | Self::CallExpr
                | Self::ApplyExpr
                | Self::FieldExpr
                | Self::IndexExpr
                | Self::RecordUpdateExpr
                | Self::TypeTestExpr
                | Self::TypeCastExpr
                | Self::PrefixExpr
                | Self::InfixExpr
                | Self::BinaryExpr
                | Self::CaseExpr
                | Self::PerformExpr
                | Self::HandleExpr
                | Self::ResumeExpr
                | Self::QuoteExpr
                | Self::SpliceExpr
                | Self::AttributedExpr
                | Self::ExportExpr
        )
    }

    #[must_use]
    pub const fn is_pat(self) -> bool {
        matches!(
            self,
            Self::WildcardPat
                | Self::BindPat
                | Self::LiteralPat
                | Self::VariantPat
                | Self::RecordPat
                | Self::TuplePat
                | Self::ArrayPat
                | Self::OrPat
                | Self::AsPat
        )
    }

    #[must_use]
    pub const fn is_ty(self) -> bool {
        matches!(
            self,
            Self::NamedTy
                | Self::FunctionTy
                | Self::BinaryTy
                | Self::PiTy
                | Self::TupleTy
                | Self::ArrayTy
        )
    }
}
