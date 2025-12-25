pub mod error;
pub mod expr;
pub mod parser;
pub mod pat;
pub mod stmt;
pub mod typ;

pub use error::ParseErrorKind;
pub use parser::Parser;

use musi_ast::Program;
use musi_basic::diagnostic::DiagnosticBag;
use musi_lex::token::Token;

#[must_use]
pub fn parse(tokens: &[Token]) -> (Program, DiagnosticBag) {
    let mut parser = Parser::new(tokens);
    let program = parser.parse_prog();
    (program, parser.diagnostics)
}
