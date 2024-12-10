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
    Star,              // *
    Slash,             // /
    Plus,              // +
    Minus,             // -
    LessLess,          // <<
    GreaterGreater,    // >>
    Less,              // <
    Greater,           // >
    LessEquals,        // <=
    GreaterEquals,     // >=
    LessEqualsGreater, // <=>
    Bang,              // !
    Ampersand,         // &
    Caret,             // ^
    Pipe,              // |
    Tilde,             // ~
    Equals,            // =
    TildeEquals,       // ~=
    SlashEquals,       // /=
    ColonEquals,       // :=
    MinusGreater,      // ->
    LessMinus,         // <-
    PipeGreater,       // |>

    // Keywords
    And,
    As,
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
    Is,
    Let,
    Not,
    Of,
    Or,
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
    pub lexeme: Vec<u8>,
    pub span: Span,
}

impl Token {
    pub fn new(kind: Kind, lexeme: &[u8], span: Span) -> Self {
        Self {
            kind,
            lexeme: lexeme.to_vec(),
            span,
        }
    }
}
