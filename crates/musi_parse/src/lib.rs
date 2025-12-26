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

use musi_ast::Prog;
use musi_basic::diagnostic::DiagnosticBag;
use musi_lex::token::Token;

#[must_use]
pub fn parse(tokens: &[Token]) -> (Prog, DiagnosticBag) {
    let mut parser = Parser::new(tokens);
    let prog = parser.parse_prog();
    (prog, parser.diagnostics)
}
