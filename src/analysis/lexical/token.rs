use crate::core::span::Span;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    // Keywords
    And,
    As,
    Break,
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
    Match,
    Not,
    Of,
    Or,
    Ref,
    Return,
    Then,
    To,
    True,
    Type,
    Until,
    Var,
    Where,
    When,
    While,
    With,
    Yield,

    Identifer,

    // Literals
    Number,
    String,
    Character,

    // Operators
    Caret,             // ^
    Star,              // *
    Slash,             // /
    Plus,              // +
    Minus,             // -
    PipeGreater,       // |>
    MinusGreater,      // ->
    LessMinus,         // <-
    LessEqualsGreater, // <=>
    GreaterEquals,     // >=
    LessEquals,        // <=
    Less,              // <
    Greater,           // >
    Pipe,              // |
    Equals,            // =
    ColonEquals,       // :=
    SlashEquals,       // /=

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

    // Layout
    Newline,
    Indent,
    Dedent,

    // Special
    Eof,
    Unknown,
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
