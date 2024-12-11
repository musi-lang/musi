use crate::core::span::Span;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    // Special
    Eof,
    Unknown,
    Newline,
    Indent,
    Dedent,
    Identifier,
    Literal(LiteralKind),

    // Operators
    Plus,              // +
    Minus,             // -
    Star,              // *
    Slash,             // /
    Caret,             // ^
    Pipe,              // |
    Less,              // <
    Greater,           // >
    Equals,            // =
    LessEquals,        // <=
    GreaterEquals,     // >=
    LessEqualsGreater, // <=>
    SlashEquals,       // /=
    ColonEquals,       // :=
    MinusGreater,      // ->
    PipeGreater,       // |>

    // Delimiters
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Comma,        // ,
    Colon,        // :
    Dot,          // .
    DotDot,       // ..
    DotDotLess,   // ..<

    // Keywords
    And,
    As,
    Break,
    Case,
    Continue,
    Deref,
    Do,
    Else,
    False,
    For,
    Forall,
    From,
    If,
    In,
    Is,
    Let,
    Not,
    Of,
    Or,
    Ref,
    Return,
    Then,
    True,
    Type,
    Where,
    While,
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
    #[must_use]
    pub fn new(kind: Kind, lexeme: &[u8], span: Span) -> Self {
        Self {
            kind,
            lexeme: lexeme.to_vec(),
            span,
        }
    }
}
