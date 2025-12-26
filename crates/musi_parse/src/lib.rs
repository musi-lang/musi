pub mod error;
pub mod expr;
pub mod parser;
pub mod pat;
pub mod stmt;
pub mod ty_expr;

#[cfg(test)]
mod tests;

pub use error::ParseErrorKind;
pub use parser::Parser;

use musi_ast::{AstArena, Prog};
use musi_basic::diagnostic::DiagnosticBag;
use musi_lex::token::Token;

pub struct ParseResult {
    pub prog: Prog,
    pub arena: AstArena,
    pub diagnostics: DiagnosticBag,
}

#[must_use]
pub fn parse(tokens: &[Token]) -> ParseResult {
    let mut arena = AstArena::new();
    let mut parser = Parser::new(tokens, &mut arena);
    let prog = parser.parse_prog();
    let diagnostics = std::mem::take(&mut parser.diagnostics);
    ParseResult {
        prog,
        arena,
        diagnostics,
    }
}
