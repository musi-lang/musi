pub mod cursor;
pub mod lexer;
pub mod token;
pub mod types;

pub use lexer::*;
use musi_basic::{interner::Interner, source::SourceFile};
pub use types::*;

use crate::token::TokenKind;

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
