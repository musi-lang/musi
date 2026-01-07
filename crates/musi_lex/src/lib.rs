pub mod cursor;
pub mod errors;
pub mod lexer;
pub mod types;

pub use musi_core::token;

pub use lexer::*;
use musi_core::{Interner, SourceFile, TokenKind};
pub use types::*;

/// Turns source file input into stream of tokens and diagnostics.
pub fn tokenize(source: &SourceFile, interner: &mut Interner) -> TokenStream {
    let mut lexer = Lexer::new(source, interner);
    let mut tokens = vec![];
    loop {
        let token = lexer.next_token();
        if token.kind == TokenKind::EOF {
            tokens.push(token);
            break;
        }
        tokens.push(token);
    }
    (tokens, lexer.errors())
}
