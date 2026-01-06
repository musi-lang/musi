mod ctx;
pub mod expr;
pub mod parser;
pub mod pat;
pub mod stmt;
pub mod ty_expr;

use musi_ast::{AstArena, Prog};
use musi_basic::interner::Interner;
use musi_errors::DiagnosticBag;
pub use musi_errors::ParseErrorKind;
use musi_lex::token::Token;
pub use parser::Parser;
use std::mem;

pub struct ParseResult {
    pub prog: Prog,
    pub arena: AstArena,
    pub diagnostics: DiagnosticBag,
}

#[must_use]
pub fn parse(tokens: &[Token], interner: &Interner) -> ParseResult {
    let mut arena = AstArena::new();
    let mut parser = Parser::new(tokens, &mut arena, interner);
    let prog = parser.parse_prog();
    let diagnostics = mem::take(&mut parser.diagnostics);

    ParseResult {
        prog,
        arena,
        diagnostics,
    }
}
