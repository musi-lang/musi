//! Token types for the Musi lexer.

use musi_shared::{Span, Symbol};

use crate::trivia::TriviaRange;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // -- Keywords ---------------------------------------------------------
    Fn,
    Const,
    Var,
    If,
    Then,
    Elif,
    Else,
    Match,
    Case,
    While,
    Loop,
    For,
    In,
    Break,
    Cycle,
    Return,
    Defer,
    Import,
    From,
    Export,
    Native,
    Opaque,
    Record,
    Choice,
    And,
    Or,
    Xor,
    Not,
    As,
    With,
    Label,
    Using,
    Class,
    Given,
    Satisfies,
    Where,
    Law,
    Shl,
    Shr,

    // -- Punctuation ------------------------------------------------------
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Semi,
    Colon,
    ColonColon,
    Dot,
    DotDot,
    DotDotLt,
    MinusGt,
    EqGt,
    LtMinus,
    ColonEq,
    Eq,
    SlashEq,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Lt,
    Gt,
    LtEq,
    GtEq,
    Pipe,
    Amp,
    Caret,
    Tilde,
    Bang,
    At,
    Hash,
    Underscore,

    // -- Compound tokens --------------------------------------------------
    DotLBracket,
    DotLBrace,
    LtDotDot,

    // -- Literals ---------------------------------------------------------
    IntLit,
    FloatLit,
    StringLit,
    CharLit,

    // -- Other ------------------------------------------------------------
    Ident,
    TyIdent,
    Eof,
    Error,
}

impl TokenKind {
    #[must_use]
    pub const fn is_keyword(self) -> bool {
        matches!(
            self,
            Self::Fn
                | Self::Const
                | Self::Var
                | Self::If
                | Self::Then
                | Self::Elif
                | Self::Else
                | Self::Match
                | Self::Case
                | Self::While
                | Self::Loop
                | Self::For
                | Self::In
                | Self::Break
                | Self::Cycle
                | Self::Return
                | Self::Defer
                | Self::Import
                | Self::From
                | Self::Export
                | Self::Native
                | Self::Opaque
                | Self::Record
                | Self::Choice
                | Self::And
                | Self::Or
                | Self::Xor
                | Self::Not
                | Self::As
                | Self::With
                | Self::Label
                | Self::Using
                | Self::Class
                | Self::Given
                | Self::Satisfies
                | Self::Where
                | Self::Law
                | Self::Shl
                | Self::Shr
        )
    }

    /// Returns the fixed source text for structural tokens, or `None` for
    /// tokens whose text varies (literals, identifiers, etc.).
    #[must_use]
    pub const fn fixed_text(self) -> Option<&'static str> {
        match self {
            // -- Keywords
            Self::Fn => Some("fn"),
            Self::Const => Some("const"),
            Self::Var => Some("var"),
            Self::If => Some("if"),
            Self::Then => Some("then"),
            Self::Elif => Some("elif"),
            Self::Else => Some("else"),
            Self::Match => Some("match"),
            Self::Case => Some("case"),
            Self::While => Some("while"),
            Self::Loop => Some("loop"),
            Self::For => Some("for"),
            Self::In => Some("in"),
            Self::Break => Some("break"),
            Self::Cycle => Some("cycle"),
            Self::Return => Some("return"),
            Self::Defer => Some("defer"),
            Self::Import => Some("import"),
            Self::From => Some("from"),
            Self::Export => Some("export"),
            Self::Native => Some("native"),
            Self::Opaque => Some("opaque"),
            Self::Record => Some("record"),
            Self::Choice => Some("choice"),
            Self::And => Some("and"),
            Self::Or => Some("or"),
            Self::Xor => Some("xor"),
            Self::Not => Some("not"),
            Self::As => Some("as"),
            Self::With => Some("with"),
            Self::Label => Some("label"),
            Self::Using => Some("using"),
            Self::Class => Some("class"),
            Self::Given => Some("given"),
            Self::Satisfies => Some("satisfies"),
            Self::Where => Some("where"),
            Self::Law => Some("law"),
            Self::Shl => Some("shl"),
            Self::Shr => Some("shr"),

            // -- Punctuation
            Self::LParen => Some("("),
            Self::RParen => Some(")"),
            Self::LBrace => Some("{"),
            Self::RBrace => Some("}"),
            Self::LBracket => Some("["),
            Self::RBracket => Some("]"),
            Self::Comma => Some(","),
            Self::Semi => Some(";"),
            Self::Colon => Some(":"),
            Self::ColonColon => Some("::"),
            Self::Dot => Some("."),
            Self::DotDot => Some(".."),
            Self::DotDotLt => Some("..<"),
            Self::MinusGt => Some("->"),
            Self::EqGt => Some("=>"),
            Self::LtMinus => Some("<-"),
            Self::ColonEq => Some(":="),
            Self::Eq => Some("="),
            Self::SlashEq => Some("/="),
            Self::Plus => Some("+"),
            Self::Minus => Some("-"),
            Self::Star => Some("*"),
            Self::Slash => Some("/"),
            Self::Percent => Some("%"),
            Self::Lt => Some("<"),
            Self::Gt => Some(">"),
            Self::LtEq => Some("<="),
            Self::GtEq => Some(">="),
            Self::Pipe => Some("|"),
            Self::Amp => Some("&"),
            Self::Caret => Some("^"),
            Self::Tilde => Some("~"),
            Self::Bang => Some("!"),
            Self::At => Some("@"),
            Self::Hash => Some("#"),
            Self::Underscore => Some("_"),

            // -- Compound tokens
            Self::DotLBracket => Some(".["),
            Self::DotLBrace => Some(".{"),
            Self::LtDotDot => Some("<.."),

            // -- Variable-text tokens
            Self::IntLit
            | Self::FloatLit
            | Self::StringLit
            | Self::CharLit
            | Self::Ident
            | Self::TyIdent
            | Self::Eof
            | Self::Error => None,
        }
    }
}

/// Maps a keyword string to its [`TokenKind`], or `None`.
#[must_use]
pub fn keyword_from_str(s: &str) -> Option<TokenKind> {
    match s {
        "fn" => Some(TokenKind::Fn),
        "const" => Some(TokenKind::Const),
        "var" => Some(TokenKind::Var),
        "if" => Some(TokenKind::If),
        "then" => Some(TokenKind::Then),
        "elif" => Some(TokenKind::Elif),
        "else" => Some(TokenKind::Else),
        "match" => Some(TokenKind::Match),
        "case" => Some(TokenKind::Case),
        "while" => Some(TokenKind::While),
        "loop" => Some(TokenKind::Loop),
        "for" => Some(TokenKind::For),
        "in" => Some(TokenKind::In),
        "break" => Some(TokenKind::Break),
        "cycle" => Some(TokenKind::Cycle),
        "return" => Some(TokenKind::Return),
        "defer" => Some(TokenKind::Defer),
        "import" => Some(TokenKind::Import),
        "from" => Some(TokenKind::From),
        "export" => Some(TokenKind::Export),
        "native" => Some(TokenKind::Native),
        "opaque" => Some(TokenKind::Opaque),
        "record" => Some(TokenKind::Record),
        "choice" => Some(TokenKind::Choice),
        "and" => Some(TokenKind::And),
        "or" => Some(TokenKind::Or),
        "xor" => Some(TokenKind::Xor),
        "not" => Some(TokenKind::Not),
        "as" => Some(TokenKind::As),
        "with" => Some(TokenKind::With),
        "label" => Some(TokenKind::Label),
        "using" => Some(TokenKind::Using),
        "class" => Some(TokenKind::Class),
        "given" => Some(TokenKind::Given),
        "satisfies" => Some(TokenKind::Satisfies),
        "where" => Some(TokenKind::Where),
        "law" => Some(TokenKind::Law),
        "shl" => Some(TokenKind::Shl),
        "shr" => Some(TokenKind::Shr),
        _ => None,
    }
}

/// A single lexed token: its kind, source span, optional interned text,
/// and trivia ranges into the `LexedSource::trivia` vec.
///
/// `symbol` is `Some` for: `Ident`, `TyIdent`, `IntLit`, `FloatLit`,
/// `StringLit`, `CharLit`. It is `None` for structural tokens.
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
