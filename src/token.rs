use super::source::Span;

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
    DotDot,       // ..
    DotDotLess,   // ..<
    Colon,        // :
    Semicolon,    // ;
    Question,     // ?
    At,           // @

    // Operators
    Star,              // *
    Slash,             // /
    Plus,              // +
    Minus,             // -
    Less,              // <
    Greater,           // >
    LessEquals,        // <=
    GreaterEquals,     // >=
    LessEqualsGreater, // <=>
    Caret,             // ^
    Pipe,              // |
    Equals,            // =
    SlashEquals,       // /=
    ColonEquals,       // :=
    MinusGreater,      // ->
    PipeGreater,       // |>

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
    Func,
    If,
    In,
    Is,
    Let,
    Not,
    Of,
    Or,
    Proc,
    Return,
    Ref,
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
