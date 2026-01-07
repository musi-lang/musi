pub mod cursor;
pub mod errors;
pub mod lexer;
pub mod types;

pub use musi_core::token;

pub use lexer::*;
use musi_core::{Interner, SourceFile, TokenKind};
pub use types::*;

/// Turns source file input into stream of tokens and diagnostics.
pub fn tokenize(source: &SourceFile, interner: &mut Interner, skip_trivia: bool) -> TokenStream {
    let mut lexer = Lexer::new(source, interner);
    let mut tokens = vec![];
    loop {
        let token = lexer.next_token();
        let is_eof = token.kind == TokenKind::EOF;
        let is_trivia = matches!(
            token.kind,
            TokenKind::Whitespace
                | TokenKind::Newline
                | TokenKind::LineComment { .. }
                | TokenKind::BlockComment { .. }
        );
        if !skip_trivia || !is_trivia {
            tokens.push(token);
        }
        if is_eof {
            break;
        }
    }
    (tokens, lexer.errors())
}
