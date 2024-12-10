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
    PipePipe,          // ||
    Tilde,             // ~
    Equals,            // =
    TildeEquals,       // ~=
    SlashEquals,       // /=
    ColonEquals,       // :=
    EqualsGreater,     // =>
    MinusGreater,      // ->
    LessMinus,         // <-
    PipeGreater,       // |>

    // Keywords
    And,
    As,
    Const,
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
    Or,
    Then,
    To,
    True,
    Type,
    Until,
    Var,
    Where,
    While,
    Xor,
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
