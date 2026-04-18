use super::SyntaxNodeKind;

#[test]
fn receiver_method_head_is_not_expression_pattern_or_type() {
    let kind = SyntaxNodeKind::ReceiverMethodHead;

    assert!(!kind.is_expr());
    assert!(!kind.is_pat());
    assert!(!kind.is_ty());
}

#[test]
fn node_category_helpers_classify_representative_nodes() {
    assert!(SyntaxNodeKind::LetExpr.is_expr());
    assert!(SyntaxNodeKind::NameExpr.is_expr());
    assert!(SyntaxNodeKind::BindPat.is_pat());
    assert!(SyntaxNodeKind::NamedTy.is_ty());

    assert!(!SyntaxNodeKind::ExportMod.is_expr());
    assert!(!SyntaxNodeKind::ParamList.is_pat());
    assert!(!SyntaxNodeKind::Member.is_ty());
}
