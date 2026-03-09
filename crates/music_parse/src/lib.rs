//! Parser for the Musi compiler.
//!
//! Consumes a flat [`Token`] slice and produces a [`ParsedModule`] with
//! arena-allocated AST nodes.

mod decl;
mod error;
mod expr;
mod parser;
mod pat;
mod ty;

use music_ast::ParsedModule;
use music_lex::token::{Token, TokenKind};
use music_shared::{DiagnosticBag, FileId, Interner};

use parser::Parser;

/// Parses a token stream into a [`ParsedModule`].
///
/// `tokens` **must** end with [`TokenKind::Eof`].
#[must_use]
pub fn parse(
    tokens: &[Token],
    file_id: FileId,
    diags: &mut DiagnosticBag,
    interner: &Interner,
) -> ParsedModule {
    let mut parser = Parser::new(tokens, file_id, diags, interner);
    parser.parse_program()
}

/// Returns `true` if `kind` can begin an expression.
#[must_use]
pub const fn can_start_expr(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Ident
            | TokenKind::IntLit
            | TokenKind::FloatLit
            | TokenKind::StringLit
            | TokenKind::RuneLit
            | TokenKind::FStringHead
            | TokenKind::LParen
            | TokenKind::LBracket
            | TokenKind::Minus
            | TokenKind::Dot
            | TokenKind::DotLBrace
            | TokenKind::HashLBracket
            | TokenKind::KwNot
            | TokenKind::KwLet
            | TokenKind::KwVar
            | TokenKind::KwReturn
            | TokenKind::KwMatch
            | TokenKind::KwDefer
            | TokenKind::KwSpawn
            | TokenKind::KwAwait
            | TokenKind::KwTry
            | TokenKind::KwImport
            | TokenKind::KwExport
            | TokenKind::KwForall
            | TokenKind::KwExists
            | TokenKind::KwClass
            | TokenKind::KwGiven
            | TokenKind::KwEffect
    )
}
