use crate::utils::Span;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
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

    // Keywords
    And,
    As,
    Async,
    Await,
    Break,
    Const,
    Continue,
    Def,
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
    Or,
    Repeat,
    Return,
    Then,
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
    String,
    Character,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: Box<str>,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: Vec<u8>, span: Span) -> Self {
        Self {
            kind,
            lexeme: String::from_utf8(lexeme)
                .expect("invalid utf-8")
                .into_boxed_str(),
            span,
        }
    }
}
