use crate::builtins::Builtins;
use crate::{SemanticModel, SymbolTable};
use musi_basic::diagnostic::DiagnosticBag;
use musi_basic::interner::Interner;
use musi_basic::source::SourceFile;

pub struct TestCtx {
    pub interner: Interner,
    pub builtins: Builtins,
}

impl TestCtx {
    pub fn new() -> Self {
        let mut interner = Interner::new();
        let builtins = Builtins::from_interner(&mut interner);
        Self { interner, builtins }
    }

    pub fn check_bind(&mut self, code: &str) -> (SemanticModel, SymbolTable, DiagnosticBag) {
        let source = SourceFile::new("test.ms".to_owned(), code.to_owned(), 0);
        let (tokens, lex_errs) = musi_lex::tokenize(&source, &mut self.interner);
        assert!(lex_errs.is_empty(), "lex errors: {lex_errs:?}");

        let parse_result = musi_parse::parse(&tokens);
        assert!(
            parse_result.diagnostics.is_empty(),
            "parse errors: {:?}",
            parse_result.diagnostics
        );

        super::bind(
            &parse_result.arena,
            &self.interner,
            &parse_result.prog,
            &self.builtins,
        )
    }
}
