use super::{Token, TokenKind, keyword_from_str};
use msc_shared::{Span, Symbol};

#[test]
fn test_keyword_from_str_returns_some_for_every_keyword() {
    let keywords = [
        ("and", TokenKind::KwAnd),
        ("as", TokenKind::KwAs),
        ("choice", TokenKind::KwChoice),
        ("class", TokenKind::KwClass),
        ("derives", TokenKind::KwDerives),
        ("effect", TokenKind::KwEffect),
        ("export", TokenKind::KwExport),
        ("if", TokenKind::KwIf),
        ("import", TokenKind::KwImport),
        ("instance", TokenKind::KwInstance),
        ("law", TokenKind::KwLaw),
        ("let", TokenKind::KwLet),
        ("match", TokenKind::KwMatch),
        ("mut", TokenKind::KwMut),
        ("not", TokenKind::KwNot),
        ("of", TokenKind::KwOf),
        ("or", TokenKind::KwOr),
        ("record", TokenKind::KwRecord),
        ("return", TokenKind::KwReturn),
        ("where", TokenKind::KwWhere),
        ("xor", TokenKind::KwXor),
    ];
    for (text, expected) in keywords {
        assert_eq!(keyword_from_str(text), Some(expected), "keyword: {text}");
    }
}

#[test]
fn test_keyword_from_str_new_keywords() {
    assert_eq!(keyword_from_str("need"), Some(TokenKind::KwNeed));
    assert_eq!(keyword_from_str("fatal"), Some(TokenKind::KwFatal));
    assert_eq!(keyword_from_str("handle"), Some(TokenKind::KwHandle));
    assert_eq!(keyword_from_str("resume"), Some(TokenKind::KwResume));
    assert_eq!(keyword_from_str("with"), Some(TokenKind::KwWith));
}

#[test]
fn test_keyword_from_str_returns_none_for_non_keyword() {
    assert_eq!(keyword_from_str("foo"), None);
    assert_eq!(keyword_from_str("bar"), None);
    assert_eq!(keyword_from_str(""), None);
    assert_eq!(keyword_from_str("AND"), None);
}

#[test]
fn test_is_keyword_true_for_keywords() {
    assert!(TokenKind::KwAnd.is_keyword());
    assert!(TokenKind::KwClass.is_keyword());
    assert!(TokenKind::KwExport.is_keyword());
    assert!(TokenKind::KwInstance.is_keyword());
    assert!(TokenKind::KwLaw.is_keyword());
    assert!(TokenKind::KwMut.is_keyword());
    assert!(TokenKind::KwLet.is_keyword());
    assert!(TokenKind::KwMatch.is_keyword());
    assert!(TokenKind::KwNeed.is_keyword());
    assert!(TokenKind::KwFatal.is_keyword());
    assert!(TokenKind::KwHandle.is_keyword());
    assert!(TokenKind::KwResume.is_keyword());
    assert!(TokenKind::KwWith.is_keyword());
}

#[test]
fn test_is_keyword_false_for_non_keywords() {
    assert!(!TokenKind::Plus.is_keyword());
    assert!(!TokenKind::LParen.is_keyword());
    assert!(!TokenKind::Ident.is_keyword());
    assert!(!TokenKind::IntLit.is_keyword());
    assert!(!TokenKind::Eof.is_keyword());
}

#[test]
fn test_fixed_text_returns_correct_strings() {
    assert_eq!(TokenKind::KwLet.fixed_text(), Some("let"));
    assert_eq!(TokenKind::Plus.fixed_text(), Some("+"));
    assert_eq!(TokenKind::DashGt.fixed_text(), Some("->"));
    assert_eq!(TokenKind::EqGt.fixed_text(), Some("=>"));
    assert_eq!(TokenKind::ColonEq.fixed_text(), Some(":="));
    assert_eq!(TokenKind::DotDotLt.fixed_text(), Some("..<"));
    assert_eq!(TokenKind::DotDotDot.fixed_text(), Some("..."));
    assert_eq!(TokenKind::SlashEq.fixed_text(), Some("/="));
    assert_eq!(TokenKind::Underscore.fixed_text(), Some("_"));
    assert_eq!(TokenKind::TildeGt.fixed_text(), Some("~>"));
    assert_eq!(TokenKind::PipeGt.fixed_text(), Some("|>"));
    assert_eq!(TokenKind::HashLBracket.fixed_text(), Some("#["));
    assert_eq!(TokenKind::LtDash.fixed_text(), Some("<-"));
    assert_eq!(TokenKind::LtColon.fixed_text(), Some("<:"));
    assert_eq!(TokenKind::Bang.fixed_text(), Some("!"));
    assert_eq!(TokenKind::BangDot.fixed_text(), Some("!."));
    assert_eq!(TokenKind::ColonQuestion.fixed_text(), Some(":?"));
    assert_eq!(TokenKind::ColonQuestionGt.fixed_text(), Some(":?>"));
    assert_eq!(TokenKind::KwNeed.fixed_text(), Some("need"));
    assert_eq!(TokenKind::KwFatal.fixed_text(), Some("fatal"));
    assert_eq!(TokenKind::KwHandle.fixed_text(), Some("handle"));
    assert_eq!(TokenKind::KwResume.fixed_text(), Some("resume"));
    assert_eq!(TokenKind::KwWith.fixed_text(), Some("with"));
}

#[test]
fn test_fixed_text_returns_none_for_variable_tokens() {
    assert_eq!(TokenKind::Ident.fixed_text(), None);
    assert_eq!(TokenKind::TyIdent.fixed_text(), None);
    assert_eq!(TokenKind::IntLit.fixed_text(), None);
    assert_eq!(TokenKind::FloatLit.fixed_text(), None);
    assert_eq!(TokenKind::StringLit.fixed_text(), None);
    assert_eq!(TokenKind::RuneLit.fixed_text(), None);
    assert_eq!(TokenKind::FStringHead.fixed_text(), None);
    assert_eq!(TokenKind::FStringMiddle.fixed_text(), None);
    assert_eq!(TokenKind::FStringTail.fixed_text(), None);
    assert_eq!(TokenKind::Error.fixed_text(), None);
    assert_eq!(TokenKind::Eof.fixed_text(), None);
}

#[test]
fn test_token_new_round_trip() {
    let span = Span::new(10, 3);
    let token = Token::new(TokenKind::Plus, span, None);
    assert_eq!(token.kind, TokenKind::Plus);
    assert_eq!(token.span, span);
    assert!(token.symbol.is_none());

    let sym = Symbol(42);
    let token = Token::new(TokenKind::Ident, Span::new(0, 5), Some(sym));
    assert_eq!(token.kind, TokenKind::Ident);
    assert_eq!(token.span, Span::new(0, 5));
    assert_eq!(token.symbol, Some(sym));
}

#[test]
fn test_display_keyword_quoted() {
    assert_eq!(TokenKind::KwLet.to_string(), "'let'");
}

#[test]
fn test_display_symbol_quoted() {
    assert_eq!(TokenKind::Plus.to_string(), "'+'");
}

#[test]
fn test_display_multi_char_symbol_quoted() {
    assert_eq!(TokenKind::DashGt.to_string(), "'->'");
}

#[test]
fn test_display_ident_unquoted() {
    assert_eq!(TokenKind::Ident.to_string(), "identifier");
}

#[test]
fn test_display_fstring_head_uses_interpolated_string() {
    assert_eq!(TokenKind::FStringHead.to_string(), "interpolated string");
}

#[test]
fn test_display_eof() {
    assert_eq!(TokenKind::Eof.to_string(), "end of file");
}
