// Tests for the parent module -- extracted from the inline test block.
use super::{Token, TokenKind, keyword_from_str};
use musi_shared::Span;

#[test]
fn keyword_from_str_returns_some_for_every_keyword() {
    let keywords = [
        ("fn", TokenKind::Fn),
        ("const", TokenKind::Const),
        ("var", TokenKind::Var),
        ("if", TokenKind::If),
        ("then", TokenKind::Then),
        ("elif", TokenKind::Elif),
        ("else", TokenKind::Else),
        ("match", TokenKind::Match),
        ("case", TokenKind::Case),
        ("while", TokenKind::While),
        ("loop", TokenKind::Loop),
        ("for", TokenKind::For),
        ("in", TokenKind::In),
        ("break", TokenKind::Break),
        ("cycle", TokenKind::Cycle),
        ("return", TokenKind::Return),
        ("defer", TokenKind::Defer),
        ("import", TokenKind::Import),
        ("from", TokenKind::From),
        ("export", TokenKind::Export),
        ("native", TokenKind::Native),
        ("opaque", TokenKind::Opaque),
        ("record", TokenKind::Record),
        ("choice", TokenKind::Choice),
        ("and", TokenKind::And),
        ("or", TokenKind::Or),
        ("xor", TokenKind::Xor),
        ("not", TokenKind::Not),
        ("as", TokenKind::As),
        ("with", TokenKind::With),
        ("label", TokenKind::Label),
        ("shl", TokenKind::Shl),
        ("shr", TokenKind::Shr),
    ];
    for (text, expected) in keywords {
        assert_eq!(keyword_from_str(text), Some(expected), "keyword: {text}");
    }
}

#[test]
fn keyword_from_str_returns_none_for_non_keyword() {
    assert_eq!(keyword_from_str("foo"), None);
    assert_eq!(keyword_from_str("bar"), None);
    assert_eq!(keyword_from_str(""), None);
    assert_eq!(keyword_from_str("FN"), None);
}

#[test]
fn is_keyword_true_for_keywords() {
    assert!(TokenKind::Fn.is_keyword());
    assert!(TokenKind::Const.is_keyword());
    assert!(TokenKind::Match.is_keyword());
    assert!(TokenKind::Shr.is_keyword());
}

#[test]
fn is_keyword_false_for_non_keywords() {
    assert!(!TokenKind::Plus.is_keyword());
    assert!(!TokenKind::LParen.is_keyword());
    assert!(!TokenKind::Ident.is_keyword());
    assert!(!TokenKind::IntLit.is_keyword());
    assert!(!TokenKind::DotLBracket.is_keyword());
    assert!(!TokenKind::Eof.is_keyword());
}

#[test]
fn fixed_text_returns_correct_strings() {
    assert_eq!(TokenKind::Fn.fixed_text(), Some("fn"));
    assert_eq!(TokenKind::Plus.fixed_text(), Some("+"));
    assert_eq!(TokenKind::MinusGt.fixed_text(), Some("->"));
    assert_eq!(TokenKind::EqGt.fixed_text(), Some("=>"));
    assert_eq!(TokenKind::ColonEq.fixed_text(), Some(":="));
    assert_eq!(TokenKind::DotLBracket.fixed_text(), Some(".["));
    assert_eq!(TokenKind::DotLBrace.fixed_text(), Some(".{"));
    assert_eq!(TokenKind::LtDotDot.fixed_text(), Some("<.."));
    assert_eq!(TokenKind::DotDotLt.fixed_text(), Some("..<"));
    assert_eq!(TokenKind::SlashEq.fixed_text(), Some("/="));
    assert_eq!(TokenKind::Underscore.fixed_text(), Some("_"));
}

#[test]
fn fixed_text_returns_none_for_variable_tokens() {
    assert_eq!(TokenKind::Ident.fixed_text(), None);
    assert_eq!(TokenKind::TyIdent.fixed_text(), None);
    assert_eq!(TokenKind::IntLit.fixed_text(), None);
    assert_eq!(TokenKind::FloatLit.fixed_text(), None);
    assert_eq!(TokenKind::StringLit.fixed_text(), None);
    assert_eq!(TokenKind::CharLit.fixed_text(), None);
    assert_eq!(TokenKind::Error.fixed_text(), None);
    assert_eq!(TokenKind::Eof.fixed_text(), None);
}

#[test]
fn token_new_round_trip() {
    let span = Span::new(10, 3);
    let token = Token::new(TokenKind::Plus, span, None);
    assert_eq!(token.kind, TokenKind::Plus);
    assert_eq!(token.span, span);
    assert!(token.symbol.is_none());

    let sym = musi_shared::Symbol(42);
    let token = Token::new(TokenKind::Ident, Span::new(0, 5), Some(sym));
    assert_eq!(token.kind, TokenKind::Ident);
    assert_eq!(token.span, Span::new(0, 5));
    assert_eq!(token.symbol, Some(sym));
}
