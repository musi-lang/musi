#![allow(unused_imports)]

use crate::{Lexer, TokenKind};

use super::canonical_name_text;

mod success {
    use super::*;

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
}

mod failure {}
