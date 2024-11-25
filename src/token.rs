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

    // Operators
    Plus,                 // +
    PlusPlus,             // ++
    Minus,                // -
    Star,                 // *
    StarStar,             // **
    Slash,                // /
    SlashSlash,           // //
    Percent,              // %
    Ampersand,            // &
    Pipe,                 // |
    Caret,                // ^
    Tilde,                // ~
    Less,                 // <
    LessLess,             // <<
    LessGreater,          // <>
    Greater,              // >
    GreaterGreater,       // >>
    MinusGreater,         // ->
    EqualsGreater,        // =>
    PipeGreater,          // |>
    Equals,               // =
    PlusEquals,           // +=
    MinusEquals,          // -=
    StarEquals,           // *=
    StarStarEquals,       // **=
    SlashEquals,          // /=
    SlashSlashEquals,     // //=
    PercentEquals,        // %=
    AmpersandEquals,      // &=
    PipeEquals,           // |=
    CaretEquals,          // ^=
    LessLessEquals,       // <<=
    GreaterGreaterEquals, // >>=
    ColonEquals,          // :=
    EqualsEquals,         // ==
    LessEquals,           // <=
    LessEqualsGreater,    // <=>
    GreaterEquals,        // >=
    TildeEquals,          // ~=

    // Keywords
    And,
    At,
    As,
    Async,
    Await,
    Break,
    Const,
    Continue,
    Def,
    Deref,
    Do,
    Else,
    False,
    For,
    Foreign,
    From,
    If,
    Import,
    In,
    Inline,
    Is,
    Let,
    Match,
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
    When,
    Where,
    While,
    With,
    Yield,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LiteralKind {
    Number,

    ByteString,
    String,
    ByteCharacter,
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
