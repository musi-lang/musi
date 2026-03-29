#[cfg(test)]
mod tests {
    use music_basic::SourceMap;
    use music_lex::Lexer;
    use music_parse::parse;

    #[test]
    fn test_parse_recovery_keeps_tree() {
        let source = "let x := ; let y := z;";
        let mut sources = SourceMap::default();
        let source_id = sources.add("test.ms", source);
        let lexed = Lexer::new(source).lex();
        let parsed = parse(source_id, &lexed);

        assert!(!parsed.errors().is_empty());
    }
}
