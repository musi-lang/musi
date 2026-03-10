//! Token types for the Musi lexer.

use std::fmt;

use music_shared::{Span, Symbol};

use crate::trivia::TriviaRange;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // -- keywords ------------------------------------------------------------
    KwAnd,
    KwAs,
    KwAwait,
    KwChoice,
    KwClass,
    KwDefer,
    KwEffect,
    KwExists,
    KwExport,
    KwForall,
    KwForeign,
    KwGiven,
    KwIf,
    KwImport,
    KwIn,
    KwInout,
    KwLaw,
    KwLet,
    KwMatch,
    KwNot,
    KwOf,
    KwOr,
    KwOver,
    KwRef,
    KwReturn,
    KwSpawn,
    KwTry,
    KwUnder,
    KwVar,
    KwWhere,
    KwXor,

    // -- delimiters ----------------------------------------------------------
    LParen,   // (
    RParen,   // )
    LBrace,   // {
    RBrace,   // }
    LBracket, // [
    RBracket, // ]

    // -- single-char punctuation ---------------------------------------------
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

    // -- multi-char punctuation ----------------------------------------------
    // 3-char
    DotDotDot, // ...  spread/splat
    DotDotLt,  // ..<  exclusive range

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

    // -- literals ------------------------------------------------------------
    IntLit,
    FloatLit,
    StringLit,
    RuneLit, // 'x'

    // -- f-string tokens -----------------------------------------------------
    FStringHead,   // f"...text...{
    FStringMiddle, // }...text...{
    FStringTail,   // }...text..."

    // -- identifiers ---------------------------------------------------------
    Ident,   // regular identifier
    TyIdent, // 'T, 'a — type variable

    // -- sentinel ------------------------------------------------------------
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
                | Self::KwAwait
                | Self::KwChoice
                | Self::KwClass
                | Self::KwDefer
                | Self::KwEffect
                | Self::KwExists
                | Self::KwExport
                | Self::KwForall
                | Self::KwForeign
                | Self::KwGiven
                | Self::KwIf
                | Self::KwImport
                | Self::KwIn
                | Self::KwInout
                | Self::KwLaw
                | Self::KwLet
                | Self::KwMatch
                | Self::KwNot
                | Self::KwOf
                | Self::KwOr
                | Self::KwOver
                | Self::KwRef
                | Self::KwReturn
                | Self::KwSpawn
                | Self::KwTry
                | Self::KwUnder
                | Self::KwVar
                | Self::KwWhere
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
            Self::KwAwait => Some("await"),
            Self::KwChoice => Some("choice"),
            Self::KwClass => Some("class"),
            Self::KwDefer => Some("defer"),
            Self::KwEffect => Some("effect"),
            Self::KwExists => Some("exists"),
            Self::KwExport => Some("export"),
            Self::KwForall => Some("forall"),
            Self::KwForeign => Some("foreign"),
            Self::KwGiven => Some("given"),
            Self::KwIf => Some("if"),
            Self::KwImport => Some("import"),
            Self::KwIn => Some("in"),
            Self::KwInout => Some("inout"),
            Self::KwLaw => Some("law"),
            Self::KwLet => Some("let"),
            Self::KwMatch => Some("match"),
            Self::KwNot => Some("not"),
            Self::KwOf => Some("of"),
            Self::KwOr => Some("or"),
            Self::KwOver => Some("over"),
            Self::KwRef => Some("ref"),
            Self::KwReturn => Some("return"),
            Self::KwSpawn => Some("spawn"),
            Self::KwTry => Some("try"),
            Self::KwUnder => Some("under"),
            Self::KwVar => Some("var"),
            Self::KwWhere => Some("where"),
            Self::KwXor => Some("xor"),

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
        "await" => Some(TokenKind::KwAwait),
        "choice" => Some(TokenKind::KwChoice),
        "class" => Some(TokenKind::KwClass),
        "defer" => Some(TokenKind::KwDefer),
        "effect" => Some(TokenKind::KwEffect),
        "exists" => Some(TokenKind::KwExists),
        "export" => Some(TokenKind::KwExport),
        "forall" => Some(TokenKind::KwForall),
        "foreign" => Some(TokenKind::KwForeign),
        "given" => Some(TokenKind::KwGiven),
        "if" => Some(TokenKind::KwIf),
        "import" => Some(TokenKind::KwImport),
        "in" => Some(TokenKind::KwIn),
        "inout" => Some(TokenKind::KwInout),
        "law" => Some(TokenKind::KwLaw),
        "let" => Some(TokenKind::KwLet),
        "match" => Some(TokenKind::KwMatch),
        "not" => Some(TokenKind::KwNot),
        "of" => Some(TokenKind::KwOf),
        "or" => Some(TokenKind::KwOr),
        "over" => Some(TokenKind::KwOver),
        "ref" => Some(TokenKind::KwRef),
        "return" => Some(TokenKind::KwReturn),
        "spawn" => Some(TokenKind::KwSpawn),
        "try" => Some(TokenKind::KwTry),
        "under" => Some(TokenKind::KwUnder),
        "var" => Some(TokenKind::KwVar),
        "where" => Some(TokenKind::KwWhere),
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
