use crate::kinds::SyntaxNodeKind;

#[test]
fn test_expr_kinds_include_decl_forms() {
    assert!(SyntaxNodeKind::InstanceExpr.is_expr());
    assert!(SyntaxNodeKind::LetExpr.is_expr());
    assert!(SyntaxNodeKind::ImportExpr.is_expr());
    assert!(SyntaxNodeKind::ForeignBlockExpr.is_expr());
    assert!(SyntaxNodeKind::RecordUpdateExpr.is_expr());
    assert!(SyntaxNodeKind::TypeTestExpr.is_expr());
    assert!(SyntaxNodeKind::TypeCastExpr.is_expr());
    assert!(SyntaxNodeKind::SpliceExpr.is_expr());
}

#[test]
fn test_pat_and_ty_kinds_are_not_expr_kinds() {
    assert!(SyntaxNodeKind::TuplePat.is_pat());
    assert!(SyntaxNodeKind::FunctionTy.is_ty());
    assert!(SyntaxNodeKind::BinaryTy.is_ty());
    assert!(!SyntaxNodeKind::TuplePat.is_expr());
    assert!(!SyntaxNodeKind::FunctionTy.is_expr());
}
