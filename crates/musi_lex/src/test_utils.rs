use crate::lexer::tokenize;
use crate::token::TokenKind;
use musi_basic::interner::Interner;
use musi_basic::source::SourceFile;

pub struct TestCtx {
    pub interner: Interner,
    pub source: SourceFile,
}

impl TestCtx {
    pub fn new(input: &str) -> Self {
        Self {
            interner: Interner::new(),
            source: SourceFile::new("test.ms".into(), input.into(), 0),
        }
    }
}

pub fn check(input: &str, expected: impl FnOnce(&mut Interner) -> Vec<TokenKind>) {
    let mut ctx = TestCtx::new(input);
    let mut actual = vec![];
    let (tokens, _) = tokenize(&ctx.source, &mut ctx.interner);
    for tok in tokens {
        if tok.kind == TokenKind::EOF {
            break;
        }
        actual.push(tok.kind);
    }

    let expected_tokens = expected(&mut ctx.interner);
    assert_eq!(
        actual, expected_tokens,
        "Token mismatch for input: {input:?}"
    );
}
