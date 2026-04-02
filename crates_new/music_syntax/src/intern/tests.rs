use music_base::Span;
use music_names::Interner;

use crate::{Lexer, TokenKind};

use super::{canonical_name_text, intern_name_token};

#[test]
fn canonicalizes_ident_by_passing_text_through() {
    let lexed = Lexer::new("foo").lex();
    let raw = lexed.token_text(0).expect("token text");
    assert_eq!(canonical_name_text(TokenKind::Ident, raw), "foo");
}

#[test]
fn canonicalizes_op_ident_by_stripping_parens() {
    let lexed = Lexer::new("(+)").lex();
    let raw = lexed.token_text(0).expect("token text");
    assert_eq!(canonical_name_text(TokenKind::OpIdent, raw), "+");
}

#[test]
fn interns_name_token_as_ident() {
    let lexed = Lexer::new("Type").lex();
    let mut interner = Interner::new();
    let ident = intern_name_token(&lexed, 0, &mut interner).expect("ident");
    assert_eq!(ident.span, Span::new(0, 4));
    assert_eq!(interner.resolve(ident.name), "Type");
}
