use musi_lexgen::Lexer;

#[derive(Lexer, Debug, Clone, PartialEq)]
#[lexer(skip = r"[ \t\r\n]+")]
pub enum Token {
    #[token("fn")]
    KwFn,
    #[token("val")]
    KwVal,
    #[token(":=")]
    ColonEq,
    #[token("+")]
    Plus,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(";")]
    Semicolon,
    #[token("_")]
    Underscore,
}

#[test]
fn test_keywords_and_symbols() {
    let input = "fn val := + ( ) ;";
    let tokens: Vec<_> = Token::lexer(input).filter_map(|(t, _, _)| t).collect();
    assert_eq!(
        tokens,
        vec![
            Token::KwFn,
            Token::KwVal,
            Token::ColonEq,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::Semicolon,
        ]
    );
}

#[test]
fn test_spans() {
    let input = "fn val";
    let tokens: Vec<_> = Token::lexer(input).collect();
    assert_eq!(tokens[0], (Some(Token::KwFn), 0, 2));
    assert_eq!(tokens[1], (Some(Token::KwVal), 3, 6));
}

#[test]
fn test_unknown_returns_none() {
    let input = "fn unknown_ident val";
    let tokens: Vec<_> = Token::lexer(input).collect();
    assert_eq!(tokens[0], (Some(Token::KwFn), 0, 2));
    assert_eq!(tokens[1], (None, 3, 16));
    assert_eq!(tokens[2], (Some(Token::KwVal), 17, 20));
}

#[test]
fn test_longest_match() {
    #[derive(Lexer, Debug, Clone, PartialEq)]
    #[lexer(skip = r"[ ]+")]
    pub enum MultiChar {
        #[token("..<")]
        DotDotLt,
        #[token("..")]
        DotDot,
        #[token(".")]
        Dot,
    }

    let input = "..< .. .";
    let tokens: Vec<_> = MultiChar::lexer(input).filter_map(|(t, _, _)| t).collect();
    assert_eq!(
        tokens,
        vec![MultiChar::DotDotLt, MultiChar::DotDot, MultiChar::Dot]
    );
}

#[test]
fn test_underscore_as_token() {
    let input = "_ fn";
    let tokens: Vec<_> = Token::lexer(input).filter_map(|(t, _, _)| t).collect();
    assert_eq!(tokens, vec![Token::Underscore, Token::KwFn]);
}
