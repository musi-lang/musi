use crate::span::Span;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    Eof,
    Unknown,
    Newline,
    Indent,
    Dedent,

    Identifier,
    Literal(LiteralKind),

    // Delimiters
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Comma,        // ,
    Dot,          // .
    Colon,        // :
    Semicolon,    // ;
    Question,     // ?
    At,           // @

    // Operators
    Plus,              // +
    Minus,             // -
    Star,              // *
    Slash,             // /
    Pipe,              // |
    Caret,             // ^
    Tilde,             // ~
    Greater,           // >
    Less,              // <
    LessLess,          // <<
    GreaterGreater,    // >>
    PipeGreater,       // |>
    TildeEquals,       // ~=
    Equals,            // =
    SlashEquals,       // /=
    ColonEquals,       // :=
    LessEquals,        // <=
    GreaterEquals,     // >=
    LessEqualsGreater, // <=>
    MinusGreater,      // ->
    LessMinus,         // <-

    // Keywords
    And,
    As,
    Async,
    Await,
    Break,
    Case,
    Const,
    Continue,
    Deref,
    Do,
    Downto,
    Else,
    False,
    For,
    Foreign,
    From,
    If,
    In,
    Include,
    Inherit,
    Inline,
    Is,
    Let,
    Not,
    Of,
    Or,
    Otherwise,
    Ref,
    Repeat,
    Return,
    Then,
    To,
    True,
    Type,
    Unsafe,
    Until,
    Var,
    Where,
    While,
    Xor,
    Yield,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LiteralKind {
    Number,

    String,
    Character,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: Kind,
    pub lexeme: Box<str>,
    pub span: Span,
}

impl Token {
    pub fn new(kind: Kind, lexeme: &[u8], span: Span) -> Self {
        Self {
            kind,
            lexeme: std::str::from_utf8(lexeme).expect("valid utf-8").into(),
            span,
        }
    }
}
