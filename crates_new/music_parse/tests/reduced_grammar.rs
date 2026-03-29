#[cfg(test)]
mod tests {
    use music_ast::SourceFile;
    use music_basic::SourceMap;
    use music_lex::Lexer;
    use music_parse::parse;

    #[test]
    fn test_parse_representative_module() {
        let source = r"
let id[T] (x : T) := x;
let Maybe := data { | Some : Int | None };
instance Foo { law x := x };
";
        let mut sources = SourceMap::default();
        let source_id = sources.add("test.ms", source);
        let lexed = Lexer::new(source).lex();
        let parsed = parse(source_id, &lexed);

        assert!(SourceFile::cast(parsed.tree().root()).is_some());
    }
}
