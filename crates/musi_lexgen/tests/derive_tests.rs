use musi_lexgen::Lexer;

#[derive(Lexer, Debug, Clone, PartialEq)]
#[lexer(skip = r"[ \t\r\n]+")]
pub enum Token {
    #[token("fn")]
    KwFn,
    #[token("val")]
    KwVal,
    #[token("var")]
    KwVar,
    #[token("if")]
    KwIf,
    #[token("else")]
    KwElse,
    #[token("and")]
    KwAnd,
    #[token("or")]
    KwOr,
    #[token("not")]
    KwNot,
    #[token("true")]
    KwTrue,
    #[token("false")]
    KwFalse,

    #[token("..<")]
    DotDotLt,
    #[token("..")]
    DotDot,
    #[token(".^")]
    DotCaret,
    #[token(".")]
    Dot,
    #[token("@[")]
    AtLBrack,
    #[token("@")]
    At,
    #[token("::")]
    ColonColon,
    #[token(":=")]
    ColonEq,
    #[token(":")]
    Colon,
    #[token("=>")]
    EqGt,
    #[token("=")]
    Eq,
    #[token(">=")]
    GtEq,
    #[token(">>")]
    GtGt,
    #[token(">")]
    Gt,
    #[token("<=")]
    LtEq,
    #[token("<<")]
    LtLt,
    #[token("<-")]
    LtMinus,
    #[token("<")]
    Lt,
    #[token("->")]
    MinusGt,
    #[token("-")]
    Minus,
    #[token("/=")]
    SlashEq,
    #[token("/")]
    Slash,
    #[token("**")]
    StarStar,
    #[token("*")]
    Star,
    #[token("??")]
    QuestionQuestion,
    #[token("?")]
    Question,
    #[token("|>")]
    BarGt,
    #[token("|")]
    Bar,
    #[token("&")]
    Amp,
    #[token("^")]
    Caret,
    #[token("~")]
    Tilde,
    #[token("+")]
    Plus,
    #[token("%")]
    Percent,
    #[token("$")]
    Dollar,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBrack,
    #[token("]")]
    RBrack,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token("_")]
    Underscore,
}

#[test]
fn test_all_keywords() {
    let input = "fn val var if else and or not true false";
    let tokens: Vec<_> = Token::lexer(input).filter_map(|(t, _, _)| t).collect();
    assert_eq!(
        tokens,
        vec![
            Token::KwFn,
            Token::KwVal,
            Token::KwVar,
            Token::KwIf,
            Token::KwElse,
            Token::KwAnd,
            Token::KwOr,
            Token::KwNot,
            Token::KwTrue,
            Token::KwFalse,
        ]
    );
}

#[test]
fn test_all_multi_char_symbols() {
    let input = "..< .. .^ :: := => >= >> <= << <- -> /= ** ?? |>";
    let tokens: Vec<_> = Token::lexer(input).filter_map(|(t, _, _)| t).collect();
    assert_eq!(
        tokens,
        vec![
            Token::DotDotLt,
            Token::DotDot,
            Token::DotCaret,
            Token::ColonColon,
            Token::ColonEq,
            Token::EqGt,
            Token::GtEq,
            Token::GtGt,
            Token::LtEq,
            Token::LtLt,
            Token::LtMinus,
            Token::MinusGt,
            Token::SlashEq,
            Token::StarStar,
            Token::QuestionQuestion,
            Token::BarGt,
        ]
    );
}

#[test]
fn test_all_single_char_symbols() {
    let input = ". @ : = > < - / * ? | & ^ ~ + % $ ( ) [ ] { } , ; _";
    let tokens: Vec<_> = Token::lexer(input).filter_map(|(t, _, _)| t).collect();
    assert_eq!(
        tokens,
        vec![
            Token::Dot,
            Token::At,
            Token::Colon,
            Token::Eq,
            Token::Gt,
            Token::Lt,
            Token::Minus,
            Token::Slash,
            Token::Star,
            Token::Question,
            Token::Bar,
            Token::Amp,
            Token::Caret,
            Token::Tilde,
            Token::Plus,
            Token::Percent,
            Token::Dollar,
            Token::LParen,
            Token::RParen,
            Token::LBrack,
            Token::RBrack,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
            Token::Underscore,
        ]
    );
}

#[test]
fn test_longest_match_priority() {
    let input = "..<..";
    let tokens: Vec<_> = Token::lexer(input).filter_map(|(t, _, _)| t).collect();
    assert_eq!(tokens, vec![Token::DotDotLt, Token::DotDot]);

    let input2 = "...";
    let tokens2: Vec<_> = Token::lexer(input2).filter_map(|(t, _, _)| t).collect();
    assert_eq!(tokens2, vec![Token::DotDot, Token::Dot]);

    let input3 = "@[@";
    let tokens3: Vec<_> = Token::lexer(input3).filter_map(|(t, _, _)| t).collect();
    assert_eq!(tokens3, vec![Token::AtLBrack, Token::At]);
}

#[test]
fn test_span_tracking() {
    let input = "fn val := 123";
    let tokens: Vec<_> = Token::lexer(input).collect();
    assert_eq!(tokens[0], (Some(Token::KwFn), 0, 2));
    assert_eq!(tokens[1], (Some(Token::KwVal), 3, 6));
    assert_eq!(tokens[2], (Some(Token::ColonEq), 7, 9));
    assert_eq!(tokens[3], (None, 10, 13));
}

#[test]
fn test_unknown_ident_returns_none() {
    let input = "fn myVar val";
    let tokens: Vec<_> = Token::lexer(input).collect();
    assert_eq!(tokens[0], (Some(Token::KwFn), 0, 2));
    assert_eq!(tokens[1], (None, 3, 8));
    assert_eq!(tokens[2], (Some(Token::KwVal), 9, 12));
}

#[test]
fn test_unknown_char_returns_none() {
    let input = "fn # val";
    let tokens: Vec<_> = Token::lexer(input).collect();
    assert_eq!(tokens[0], (Some(Token::KwFn), 0, 2));
    assert_eq!(tokens[1], (None, 3, 4));
    assert_eq!(tokens[2], (Some(Token::KwVal), 5, 8));
}

#[test]
fn test_whitespace_skipping() {
    let input = "  fn  \t\n  val  \r\n  ";
    let tokens: Vec<_> = Token::lexer(input).filter_map(|(t, _, _)| t).collect();
    assert_eq!(tokens, vec![Token::KwFn, Token::KwVal]);
}

#[test]
fn test_empty_input() {
    let input = "";
    let tokens: Vec<_> = Token::lexer(input).collect();
    assert!(tokens.is_empty());
}

#[test]
fn test_only_whitespace() {
    let input = "   \t\n\r\n   ";
    let tokens: Vec<_> = Token::lexer(input).collect();
    assert!(tokens.is_empty());
}

#[test]
fn test_adjacent_symbols() {
    let input = "()[]{}";
    let tokens: Vec<_> = Token::lexer(input).filter_map(|(t, _, _)| t).collect();
    assert_eq!(
        tokens,
        vec![
            Token::LParen,
            Token::RParen,
            Token::LBrack,
            Token::RBrack,
            Token::LBrace,
            Token::RBrace,
        ]
    );
}

#[test]
fn test_keyword_prefix_as_ident() {
    let input = "fn fnName valX ifelse";
    let tokens: Vec<_> = Token::lexer(input).collect();
    assert_eq!(tokens[0].0, Some(Token::KwFn));
    assert_eq!(tokens[1].0, None);
    assert_eq!(tokens[2].0, None);
    assert_eq!(tokens[3].0, None);
}

#[test]
fn test_underscore_alone_and_prefixed() {
    let input = "_ _foo __bar";
    let tokens: Vec<_> = Token::lexer(input).collect();
    assert_eq!(tokens[0].0, Some(Token::Underscore));
    assert_eq!(tokens[1].0, None);
    assert_eq!(tokens[2].0, None);
}

#[test]
fn test_slice_helper() {
    let input = "fn unknown val";
    let lexer = Token::lexer(input);
    let tokens: Vec<_> = lexer.collect();

    let lexer2 = Token::lexer(input);
    let (_, start, end) = tokens[1];
    assert_eq!(lexer2.slice(start, end), "unknown");
}

#[test]
fn test_actual_musi_snippet() {
    let input = "val x := fn(a, b) => a + b;";
    let tokens: Vec<_> = Token::lexer(input).filter_map(|(t, _, _)| t).collect();
    assert_eq!(
        tokens,
        vec![
            Token::KwVal,
            Token::ColonEq,
            Token::KwFn,
            Token::LParen,
            Token::Comma,
            Token::RParen,
            Token::EqGt,
            Token::Plus,
            Token::Semicolon,
        ]
    );
}
