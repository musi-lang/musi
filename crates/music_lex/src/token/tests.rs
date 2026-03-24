use super::*;

#[test]
fn keyword_from_str_all() {
    let keywords = [
        ("and", TokenKind::KwAnd),
        ("as", TokenKind::KwAs),
        ("choice", TokenKind::KwChoice),
        ("class", TokenKind::KwClass),
        ("effect", TokenKind::KwEffect),
        ("export", TokenKind::KwExport),
        ("foreign", TokenKind::KwForeign),
        ("handle", TokenKind::KwHandle),
        ("if", TokenKind::KwIf),
        ("import", TokenKind::KwImport),
        ("in", TokenKind::KwIn),
        ("instance", TokenKind::KwInstance),
        ("law", TokenKind::KwLaw),
        ("let", TokenKind::KwLet),
        ("match", TokenKind::KwMatch),
        ("mut", TokenKind::KwMut),
        ("need", TokenKind::KwNeed),
        ("not", TokenKind::KwNot),
        ("of", TokenKind::KwOf),
        ("opaque", TokenKind::KwOpaque),
        ("or", TokenKind::KwOr),
        ("quote", TokenKind::KwQuote),
        ("record", TokenKind::KwRecord),
        ("resume", TokenKind::KwResume),
        ("return", TokenKind::KwReturn),
        ("via", TokenKind::KwVia),
        ("where", TokenKind::KwWhere),
        ("with", TokenKind::KwWith),
        ("xor", TokenKind::KwXor),
    ];
    assert_eq!(keywords.len(), 29);
    for (s, expected) in &keywords {
        assert_eq!(
            keyword_from_str(s),
            Some(expected.clone()),
            "keyword_from_str({s:?}) failed"
        );
    }
}

#[test]
fn keyword_from_str_non_keywords() {
    assert_eq!(keyword_from_str("foo"), None);
    assert_eq!(keyword_from_str("fn"), None);
    assert_eq!(keyword_from_str("else"), None);
    assert_eq!(keyword_from_str("while"), None);
}

#[test]
fn keyword_from_str_partial_matches() {
    assert_eq!(keyword_from_str("an"), None);
    assert_eq!(keyword_from_str("android"), None);
    assert_eq!(keyword_from_str("letting"), None);
    assert_eq!(keyword_from_str("i"), None);
}

#[test]
fn single_char_delimiters() {
    assert_eq!(single_char_token('('), Some(TokenKind::LParen));
    assert_eq!(single_char_token(')'), Some(TokenKind::RParen));
    assert_eq!(single_char_token('['), Some(TokenKind::LBracket));
    assert_eq!(single_char_token(']'), Some(TokenKind::RBracket));
    assert_eq!(single_char_token('{'), Some(TokenKind::LBrace));
    assert_eq!(single_char_token('}'), Some(TokenKind::RBrace));
}

#[test]
fn single_char_punctuation() {
    assert_eq!(single_char_token(';'), Some(TokenKind::Semi));
    assert_eq!(single_char_token(','), Some(TokenKind::Comma));
    assert_eq!(single_char_token('.'), Some(TokenKind::Dot));
    assert_eq!(single_char_token(':'), Some(TokenKind::Colon));
    assert_eq!(single_char_token('|'), Some(TokenKind::Pipe));
    assert_eq!(single_char_token('!'), Some(TokenKind::Bang));
    assert_eq!(single_char_token('?'), Some(TokenKind::Question));
    assert_eq!(single_char_token('<'), Some(TokenKind::Lt));
    assert_eq!(single_char_token('>'), Some(TokenKind::Gt));
    assert_eq!(single_char_token('='), Some(TokenKind::Eq));
}

#[test]
fn single_char_operators() {
    assert_eq!(single_char_token('+'), Some(TokenKind::Plus));
    assert_eq!(single_char_token('-'), Some(TokenKind::Minus));
    assert_eq!(single_char_token('*'), Some(TokenKind::Star));
    assert_eq!(single_char_token('/'), Some(TokenKind::Slash));
    assert_eq!(single_char_token('%'), Some(TokenKind::Percent));
}

#[test]
fn single_char_special() {
    assert_eq!(single_char_token('@'), Some(TokenKind::At));
    assert_eq!(single_char_token('#'), Some(TokenKind::Hash));
}

#[test]
fn single_char_unknown() {
    assert_eq!(single_char_token('~'), None);
    assert_eq!(single_char_token('$'), None);
    assert_eq!(single_char_token('\\'), None);
}
