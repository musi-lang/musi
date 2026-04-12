#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    pub fn is_expr(self) -> bool {
        self >= Self::SequenceExpr && self <= Self::AttributedExpr
    }

    #[must_use]
    pub fn is_pat(self) -> bool {
        self >= Self::WildcardPat && self <= Self::AsPat
    }

    #[must_use]
    pub fn is_ty(self) -> bool {
        self >= Self::NamedTy && self <= Self::ArrayTy
    }
}
