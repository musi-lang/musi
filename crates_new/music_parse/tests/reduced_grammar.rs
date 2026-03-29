#[cfg(test)]
mod tests {
    use music_ast::{Expr, ExprKindView, SourceFile, SyntaxNode};
    use music_basic::SourceMap;
    use music_lex::Lexer;
    use music_parse::parse;

    fn parse_fixture(name: &str, source: &str) -> music_parse::ParsedSource {
        let mut sources = SourceMap::default();
        let source_id = sources.add(name, source);
        let lexed = Lexer::new(source).lex();
        parse(source_id, &lexed)
    }

    #[test]
    fn test_parse_grammar_fixtures_without_errors() {
        let fixtures = [
            (
                "module_showcase.ms",
                include_str!("fixtures/module_showcase.ms"),
            ),
            ("quote_splice.ms", include_str!("fixtures/quote_splice.ms")),
            (
                "foreign_bindings.ms",
                include_str!("fixtures/foreign_bindings.ms"),
            ),
            ("operators.ms", include_str!("fixtures/operators.ms")),
            ("types.ms", include_str!("fixtures/types.ms")),
        ];

        for (name, source) in fixtures {
            let parsed = parse_fixture(name, source);
            assert!(
                parsed.errors().is_empty(),
                "{name} had parser errors: {:?}",
                parsed.errors()
            );
            assert!(
                SourceFile::cast(parsed.tree().root()).is_some(),
                "{name} root"
            );
        }
    }

    #[test]
    fn test_quote_splice_fixture_contains_splice_expr() {
        let parsed = parse_fixture("quote_splice.ms", include_str!("fixtures/quote_splice.ms"));
        let root = SourceFile::cast(parsed.tree().root()).expect("source file");

        assert!(contains_splice(root.syntax()));
    }

    fn contains_splice(node: SyntaxNode<'_>) -> bool {
        node.child_nodes().any(|child| {
            Expr::cast(child).is_some_and(|expr| expr.kind() == ExprKindView::Splice)
                || contains_splice(child)
        })
    }
}
