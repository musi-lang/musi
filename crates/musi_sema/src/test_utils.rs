use crate::SemanticModel;
use crate::binder::bind;
use crate::builtins::Builtins;
use musi_basic::diagnostic::DiagnosticBag;
use musi_basic::interner::Interner;
use musi_basic::source::SourceFile;
use musi_lex::lexer::tokenize;
use musi_parse::parse;

pub struct TestContext {
    pub interner: Interner,
    pub builtins: Builtins,
}

impl TestContext {
    pub fn new() -> Self {
        let mut interner = Interner::new();
        let builtins = Builtins::from_interner(&mut interner);
        Self { interner, builtins }
    }

    pub fn check_bind(&mut self, code: &str) -> (SemanticModel, DiagnosticBag) {
        let source = SourceFile::new("test.ms".to_string(), code.to_string(), 0);
        let (tokens, lex_errs) = tokenize(&source, &mut self.interner);
        assert!(lex_errs.is_empty(), "lex errors: {lex_errs:?}");

        let parse_result = parse(&tokens);
        assert!(
            parse_result.diagnostics.is_empty(),
            "parse errors: {:?}",
            parse_result.diagnostics
        );

        bind(
            &parse_result.arena,
            &self.interner,
            &parse_result.prog,
            &self.builtins,
        )
    }
}
