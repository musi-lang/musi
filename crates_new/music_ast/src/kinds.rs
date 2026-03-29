#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SyntaxNodeKind {
    Error,
    SourceFile,
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
    TupleExpr,
    ArrayExpr,
    RecordExpr,
    VariantExpr,
    LambdaExpr,
    CallExpr,
    FieldExpr,
    IndexExpr,
    RecordUpdateExpr,
    TypeTestExpr,
    TypeCastExpr,
    PrefixExpr,
    BinaryExpr,
    CaseExpr,
    PerformExpr,
    HandleExpr,
    ResumeExpr,
    QuoteExpr,

    WildcardPat,
    BindPat,
    LiteralPat,
    VariantPat,
    RecordPat,
    TuplePat,
    ArrayPat,
    OrPat,

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
    ImportTarget,
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
                | Self::TupleExpr
                | Self::ArrayExpr
                | Self::RecordExpr
                | Self::VariantExpr
                | Self::LambdaExpr
                | Self::CallExpr
                | Self::FieldExpr
                | Self::IndexExpr
                | Self::RecordUpdateExpr
                | Self::TypeTestExpr
                | Self::TypeCastExpr
                | Self::PrefixExpr
                | Self::BinaryExpr
                | Self::CaseExpr
                | Self::PerformExpr
                | Self::HandleExpr
                | Self::ResumeExpr
                | Self::QuoteExpr
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

#[cfg(test)]
mod tests;
