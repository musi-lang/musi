mod errors;
mod expr;
mod parser;
mod pat;
mod ty;

pub use errors::{ParseError, ParseErrorKind, ParseResult};

use music_ast::data::AstData;
use music_found::Interner;
use music_lex::Token;

use parser::Parser;

/// Parse a token stream into an AST.
///
/// Returns the AST data and any errors encountered during parsing.
/// Recovers from errors at statement boundaries so a single error
/// does not abort the entire parse.
#[must_use]
pub fn parse(
    tokens: &[Token],
    source: &str,
    interner: &mut Interner,
) -> (AstData, Vec<ParseError>) {
    let mut parser = Parser::new(tokens, source, interner);
    parser.parse_root();
    parser.finish()
}
