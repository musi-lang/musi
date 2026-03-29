use music_ast::{
    Expr, ExprKindView, SourceFile, SyntaxElementId, SyntaxNodeKind, SyntaxTreeBuilder,
};
use music_basic::{SourceMap, Span};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_file_is_expression_sequence_and_instance_is_an_expression() {
        let mut sources = SourceMap::default();
        let source_id = sources.add("test.ms", "instance Foo; let _ := instance Foo");
        let mut builder = SyntaxTreeBuilder::new(source_id);

        let first_expr = builder.push_empty_node(SyntaxNodeKind::InstanceExpr, Span::new(0, 12));
        let second_expr = builder.push_empty_node(SyntaxNodeKind::LetExpr, Span::new(14, 35));
        let root_id = builder.push_node(
            SyntaxNodeKind::SourceFile,
            Span::new(0, 35),
            [
                SyntaxElementId::Node(first_expr),
                SyntaxElementId::Node(second_expr),
            ],
        );

        let tree = builder.finish(root_id);
        let root = SourceFile::cast(tree.root()).expect("source file");
        let kinds: Vec<_> = root.expressions().map(Expr::kind).collect();

        assert_eq!(kinds, vec![ExprKindView::Instance, ExprKindView::Let]);
    }
}
