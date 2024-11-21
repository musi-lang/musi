use crate::span::Span;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenKind {
    // Keywords
    And,
    As,
    Async,
    Await,
    Break,
    Class,
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
    Implements,
    Import,
    In,
    Inline,
    Is,
    Inherits,
    Let,
    Match,
    New,
    Not,
    Or,
    Override,
    Protected,
    Public,
    Ref,
    Repeat,
    Return,
    Self_,
    Super,
    Then,
    Trait,
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

    // Delimiters
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Colon,        // :
    Comma,        // ,
    Dot,          // .

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
    Equals,               // =
    Less,                 // <
    LessLess,             // <<
    Greater,              // >
    GreaterGreater,       // >>
    DotDot,               // ..
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
    EqualsEquals,         // ==
    BangEquals,           // !=
    LessEquals,           // <=
    LessEqualsLess,       // <=>
    LessLessEquals,       // <<=
    GreaterEquals,        // >=
    GreaterGreaterEquals, // >>=
    ColonEquals,          // :=
    DotDotEquals,         // ..=

    Identifier,
    NumberLiteral,
    StringLiteral,
    CharacterLiteral,

    Dedent,
    Indent,
    Newline,
    Eof,
    Unknown,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub span: Span,
}
impl Token {
    pub const fn new(kind: TokenKind, lexeme: String, span: Span) -> Self {
        Self { kind, lexeme, span }
    }

    pub fn synthetic(kind: TokenKind) -> Self {
        Self {
            kind,
            lexeme: String::new(),
            span: Span::default(),
        }
    }
}
