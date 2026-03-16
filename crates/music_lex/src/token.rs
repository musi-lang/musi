//! Token types for the Musi lexer.

use std::fmt;

use music_shared::{Span, Symbol};

use crate::trivia::TriviaRange;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // Keywords (alphabetically)
    KwAnd,
    KwAs,
    KwChoice,
    KwClass,
    KwDefer,
    KwDo,
    KwEffect,
    KwExport,
    KwFatal,
    KwForeign,
    KwHandle,
    KwIf,
    KwImport,
    KwIn,
    KwInstance,
    KwLaw,
    KwLet,
    KwMatch,
    KwMut,
    KwNot,
    KwOf,
    KwOr,
    KwRecord,
    KwResume,
    KwReturn,
    KwTry,
    KwWhere,
    KwWith,
    KwXor,

    // Delimiters
    LParen,   // (
    RParen,   // )
    LBrace,   // {
    RBrace,   // }
    LBracket, // [
    RBracket, // ]

    // Single-char punctuation
    Plus,       // +
    Minus,      // -
    Star,       // *
    Slash,      // /
    Percent,    // %
    Eq,         // =
    Lt,         // <
    Gt,         // >
    Dot,        // .
    Comma,      // ,
    Colon,      // :
    Semi,       // ;
    Pipe,       // |
    Question,   // ?
    Underscore, // _

    // Multi-char punctuation
    // 3-char
    DotDotDot, // ...  spread/splat
    DotDotLt,  // ..<  exclusive range
    // 2-char (bang)
    BangBang, // !!   force null coalesce
    BangDot,  // !.   forced unwrap field access
    // 1-char (bang)
    Bang, // !    force unwrap postfix
    // 2-char (colon-question)
    ColonQuestionGt, // :?>  type cast
    ColonQuestion,   // :?   type test
    // 2-char
    ColonColon,       // ::   cons
    ColonEq,          // :=   binding
    ColonGt,          // :>   supertype constraint
    DotDot,           // ..   inclusive range
    LtDash,           // <-   mutation
    DashGt,           // ->   pure function arrow
    TildeGt,          // ~>   effectful function arrow
    LtColon,          // <:   subtype constraint
    QuestionDot,      // ?.   optional chain
    QuestionQuestion, // ??   nil coalesce
    LtLt,             // <<   left shift
    GtGt,             // >>   right shift
    SlashEq,          // /=   inequality
    LtEq,             // <=   less-or-equal
    GtEq,             // >=   greater-or-equal
    PipeGt,           // |>   pipe
    DotLBracket,      // .[   subscript access
    DotLBrace,        // .{   record literal
    EqGt,             // =>   fat arrow (match arms)
    HashLBracket,     // #[   attribute start

    // Literals
    IntLit,
    FloatLit,
    StringLit,
    RuneLit, // 'x'

    // F-string tokens
    FStringHead,   // f"...text...{
    FStringMiddle, // }...text...{
    FStringTail,   // }...text..."

    // Identifiers
    Ident,   // regular identifier
    TyIdent, // 'T, 'a — type variable

    // Sentinel
    Eof,
    Error,
}

impl TokenKind {
    #[must_use]
    pub const fn is_keyword(self) -> bool {
        matches!(
            self,
            Self::KwAnd
                | Self::KwAs
                | Self::KwChoice
                | Self::KwClass
                | Self::KwDefer
                | Self::KwDo
                | Self::KwEffect
                | Self::KwExport
                | Self::KwFatal
                | Self::KwForeign
                | Self::KwHandle
                | Self::KwIf
                | Self::KwImport
                | Self::KwIn
                | Self::KwInstance
                | Self::KwLaw
                | Self::KwLet
                | Self::KwMatch
                | Self::KwMut
                | Self::KwNot
                | Self::KwOf
                | Self::KwOr
                | Self::KwRecord
                | Self::KwResume
                | Self::KwReturn
                | Self::KwTry
                | Self::KwWhere
                | Self::KwWith
                | Self::KwXor
        )
    }

    /// Returns the fixed source text for structural tokens, or `None` for
    /// tokens whose text varies (literals, identifiers, etc.).
    #[must_use]
    pub const fn fixed_text(self) -> Option<&'static str> {
        match self {
            Self::KwAnd => Some("and"),
            Self::KwAs => Some("as"),
            Self::KwChoice => Some("choice"),
            Self::KwClass => Some("class"),
            Self::KwDefer => Some("defer"),
            Self::KwDo => Some("do"),
            Self::KwEffect => Some("effect"),
            Self::KwExport => Some("export"),
            Self::KwFatal => Some("fatal"),
            Self::KwForeign => Some("foreign"),
            Self::KwHandle => Some("handle"),
            Self::KwIf => Some("if"),
            Self::KwImport => Some("import"),
            Self::KwIn => Some("in"),
            Self::KwInstance => Some("instance"),
            Self::KwLaw => Some("law"),
            Self::KwLet => Some("let"),
            Self::KwMatch => Some("match"),
            Self::KwMut => Some("mut"),
            Self::KwNot => Some("not"),
            Self::KwOf => Some("of"),
            Self::KwOr => Some("or"),
            Self::KwRecord => Some("record"),
            Self::KwResume => Some("resume"),
            Self::KwReturn => Some("return"),
            Self::KwTry => Some("try"),
            Self::KwWhere => Some("where"),
            Self::KwWith => Some("with"),
            Self::KwXor => Some("xor"),
            Self::Bang => Some("!"),
            Self::BangBang => Some("!!"),
            Self::BangDot => Some("!."),
            Self::ColonQuestion => Some(":?"),
            Self::ColonQuestionGt => Some(":?>"),

            Self::LParen => Some("("),
            Self::RParen => Some(")"),
            Self::LBrace => Some("{"),
            Self::RBrace => Some("}"),
            Self::LBracket => Some("["),
            Self::RBracket => Some("]"),

            Self::Plus => Some("+"),
            Self::Minus => Some("-"),
            Self::Star => Some("*"),
            Self::Slash => Some("/"),
            Self::Percent => Some("%"),
            Self::Eq => Some("="),
            Self::Lt => Some("<"),
            Self::Gt => Some(">"),
            Self::Dot => Some("."),
            Self::Comma => Some(","),
            Self::Colon => Some(":"),
            Self::Semi => Some(";"),
            Self::Pipe => Some("|"),
            Self::Question => Some("?"),
            Self::Underscore => Some("_"),

            Self::DotDotDot => Some("..."),
            Self::DotDotLt => Some("..<"),
            Self::ColonColon => Some("::"),
            Self::ColonEq => Some(":="),
            Self::ColonGt => Some(":>"),
            Self::DotDot => Some(".."),
            Self::LtDash => Some("<-"),
            Self::DashGt => Some("->"),
            Self::TildeGt => Some("~>"),
            Self::LtColon => Some("<:"),
            Self::QuestionDot => Some("?."),
            Self::QuestionQuestion => Some("??"),
            Self::LtLt => Some("<<"),
            Self::GtGt => Some(">>"),
            Self::SlashEq => Some("/="),
            Self::LtEq => Some("<="),
            Self::GtEq => Some(">="),
            Self::PipeGt => Some("|>"),
            Self::DotLBracket => Some(".["),
            Self::DotLBrace => Some(".{"),
            Self::EqGt => Some("=>"),
            Self::HashLBracket => Some("#["),

            Self::IntLit
            | Self::FloatLit
            | Self::StringLit
            | Self::RuneLit
            | Self::FStringHead
            | Self::FStringMiddle
            | Self::FStringTail
            | Self::Ident
            | Self::TyIdent
            | Self::Eof
            | Self::Error => None,
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident => f.write_str("identifier"),
            Self::TyIdent => f.write_str("type variable"),
            Self::IntLit => f.write_str("integer literal"),
            Self::FloatLit => f.write_str("float literal"),
            Self::StringLit => f.write_str("string literal"),
            Self::RuneLit => f.write_str("rune literal"),
            Self::FStringHead => f.write_str("interpolated string"),
            Self::FStringMiddle => f.write_str("interpolated string segment"),
            Self::FStringTail => f.write_str("interpolated string end"),
            Self::Eof => f.write_str("end of file"),
            Self::Error => f.write_str("error"),
            other => {
                let text = other.fixed_text().unwrap_or("token");
                write!(f, "'{text}'")
            }
        }
    }
}

/// Maps a keyword string to its [`TokenKind`], or `None`.
#[must_use]
pub fn keyword_from_str(s: &str) -> Option<TokenKind> {
    match s {
        "and" => Some(TokenKind::KwAnd),
        "as" => Some(TokenKind::KwAs),
        "choice" => Some(TokenKind::KwChoice),
        "class" => Some(TokenKind::KwClass),
        "defer" => Some(TokenKind::KwDefer),
        "do" => Some(TokenKind::KwDo),
        "effect" => Some(TokenKind::KwEffect),
        "export" => Some(TokenKind::KwExport),
        "fatal" => Some(TokenKind::KwFatal),
        "foreign" => Some(TokenKind::KwForeign),
        "handle" => Some(TokenKind::KwHandle),
        "if" => Some(TokenKind::KwIf),
        "import" => Some(TokenKind::KwImport),
        "in" => Some(TokenKind::KwIn),
        "instance" => Some(TokenKind::KwInstance),
        "law" => Some(TokenKind::KwLaw),
        "let" => Some(TokenKind::KwLet),
        "match" => Some(TokenKind::KwMatch),
        "mut" => Some(TokenKind::KwMut),
        "not" => Some(TokenKind::KwNot),
        "of" => Some(TokenKind::KwOf),
        "or" => Some(TokenKind::KwOr),
        "record" => Some(TokenKind::KwRecord),
        "resume" => Some(TokenKind::KwResume),
        "return" => Some(TokenKind::KwReturn),
        "try" => Some(TokenKind::KwTry),
        "where" => Some(TokenKind::KwWhere),
        "with" => Some(TokenKind::KwWith),
        "xor" => Some(TokenKind::KwXor),
        _ => None,
    }
}

/// A single lexed token: its kind, source span, optional interned text,
/// and trivia ranges into the `LexedSource::trivia` vec.
#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub symbol: Option<Symbol>,
    pub leading_trivia: TriviaRange,
    pub trailing_trivia: TriviaRange,
}

impl Token {
    #[must_use]
    pub const fn new(kind: TokenKind, span: Span, symbol: Option<Symbol>) -> Self {
        Self {
            kind,
            span,
            symbol,
            leading_trivia: TriviaRange { start: 0, len: 0 },
            trailing_trivia: TriviaRange { start: 0, len: 0 },
        }
    }
}

#[cfg(test)]
mod tests;
