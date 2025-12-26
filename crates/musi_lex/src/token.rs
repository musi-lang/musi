use musi_basic::interner::Interner;
use musi_basic::span::Span;
use std::borrow::Cow;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum TokenKind {
    Ident(u32),
    LitInt(u32),
    LitReal(u32),
    LitString(u32),
    LitRune(char),
    LitTemplateNoSubst(u32),
    TemplateHead(u32),
    TemplateMiddle(u32),
    TemplateTail(u32),

    // Keywords (alphabetically)
    KwAlias,
    KwAnd,
    KwAs,
    KwBreak,
    KwCase,
    KwCycle,
    KwDefer,
    KwElse,
    KwExport,
    KwExtern,
    KwFalse,
    KwFn,
    KwFor,
    KwIf,
    KwImport,
    KwIn,
    KwIs,
    KwMatch,
    KwMod,
    KwNot,
    KwOr,
    KwRecord,
    KwReturn,
    KwSum,
    KwTrue,
    KwTry,
    KwUnsafe,
    KwVal,
    KwVar,
    KwWhile,
    KwWith,

    // Symbols
    LBrace,
    RBrace,
    LBrack,
    RBrack,
    LParen,
    RParen,
    AtLBrack,
    Comma,
    Dot,
    Colon,
    Semicolon,
    Eq,
    SlashEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    LtLt,
    GtGt,
    StarStar,
    Amp,
    Bar,
    BarGt,
    Caret,
    Tilde,
    At,
    DotCaret,
    ColonColon,
    QuestionQuestion,
    DotDot,
    DotDotLt,
    MinusGt,
    LtMinus,
    ColonEq,
    EqGt,
    Question,
    Underscore,
    Dollar,

    // Special
    EOF,
    Invalid(u32),
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    #[must_use]
    pub const fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    #[must_use]
    pub fn dummy() -> Self {
        Self {
            kind: TokenKind::EOF,
            span: Span::default(),
        }
    }
}

pub const KEYWORDS: &[(&str, TokenKind)] = &[
    ("alias", TokenKind::KwAlias),
    ("and", TokenKind::KwAnd),
    ("as", TokenKind::KwAs),
    ("break", TokenKind::KwBreak),
    ("case", TokenKind::KwCase),
    ("cycle", TokenKind::KwCycle),
    ("defer", TokenKind::KwDefer),
    ("else", TokenKind::KwElse),
    ("export", TokenKind::KwExport),
    ("extern", TokenKind::KwExtern),
    ("false", TokenKind::KwFalse),
    ("fn", TokenKind::KwFn),
    ("for", TokenKind::KwFor),
    ("if", TokenKind::KwIf),
    ("import", TokenKind::KwImport),
    ("in", TokenKind::KwIn),
    ("is", TokenKind::KwIs),
    ("match", TokenKind::KwMatch),
    ("mod", TokenKind::KwMod),
    ("not", TokenKind::KwNot),
    ("or", TokenKind::KwOr),
    ("record", TokenKind::KwRecord),
    ("return", TokenKind::KwReturn),
    ("sum", TokenKind::KwSum),
    ("true", TokenKind::KwTrue),
    ("unsafe", TokenKind::KwUnsafe),
    ("val", TokenKind::KwVal),
    ("var", TokenKind::KwVar),
    ("while", TokenKind::KwWhile),
    ("with", TokenKind::KwWith),
];

pub const SYMBOLS: &[(TokenKind, &str)] = &[
    (TokenKind::LBrace, "{"),
    (TokenKind::RBrace, "}"),
    (TokenKind::LBrack, "["),
    (TokenKind::RBrack, "]"),
    (TokenKind::LParen, "("),
    (TokenKind::RParen, ")"),
    (TokenKind::AtLBrack, "@["),
    (TokenKind::Comma, ","),
    (TokenKind::Dot, "."),
    (TokenKind::Colon, ":"),
    (TokenKind::Semicolon, ";"),
    (TokenKind::Eq, "="),
    (TokenKind::SlashEq, "/="),
    (TokenKind::Lt, "<"),
    (TokenKind::LtEq, "<="),
    (TokenKind::Gt, ">"),
    (TokenKind::GtEq, ">="),
    (TokenKind::Plus, "+"),
    (TokenKind::Minus, "-"),
    (TokenKind::Star, "*"),
    (TokenKind::Slash, "/"),
    (TokenKind::Percent, "%"),
    (TokenKind::LtLt, "<<"),
    (TokenKind::GtGt, ">>"),
    (TokenKind::StarStar, "**"),
    (TokenKind::Amp, "&"),
    (TokenKind::Bar, "|"),
    (TokenKind::BarGt, "|>"),
    (TokenKind::Caret, "^"),
    (TokenKind::Tilde, "~"),
    (TokenKind::At, "@"),
    (TokenKind::DotCaret, ".^"),
    (TokenKind::ColonColon, "::"),
    (TokenKind::QuestionQuestion, "??"),
    (TokenKind::DotDot, ".."),
    (TokenKind::DotDotLt, "..<"),
    (TokenKind::MinusGt, "->"),
    (TokenKind::LtMinus, "<-"),
    (TokenKind::ColonEq, ":="),
    (TokenKind::EqGt, "=>"),
    (TokenKind::Question, "?"),
    (TokenKind::Underscore, "_"),
    (TokenKind::Dollar, "$"),
    (TokenKind::EOF, "EOF"),
];

#[derive(Debug)]
pub struct TokenDisplay<'a> {
    kind: &'a TokenKind,
    interner: &'a Interner,
}

impl TokenKind {
    #[must_use]
    pub const fn display<'a>(&'a self, interner: &'a Interner) -> TokenDisplay<'a> {
        TokenDisplay {
            kind: self,
            interner,
        }
    }

    #[must_use]
    pub fn as_str(&self) -> Cow<'static, str> {
        for (kw, tk) in KEYWORDS {
            if *tk == *self {
                return Cow::Borrowed(kw);
            }
        }
        for (tk, sym) in SYMBOLS {
            if *tk == *self {
                return Cow::Borrowed(sym);
            }
        }
        Cow::Owned(format!("{self:?}"))
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl fmt::Display for TokenDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            TokenKind::Ident(id) => {
                write!(f, "{}", self.interner.lookup(*id).unwrap_or("<ident>"))
            }
            TokenKind::LitInt(id) => write!(f, "{}", self.interner.lookup(*id).unwrap_or("<int>")),
            TokenKind::LitReal(id) => {
                write!(f, "{}", self.interner.lookup(*id).unwrap_or("<real>"))
            }
            TokenKind::LitString(id) => {
                write!(f, "\"{}\"", self.interner.lookup(*id).unwrap_or(""))
            }
            TokenKind::LitRune(c) => write!(f, "'{c}''"),
            TokenKind::LitTemplateNoSubst(id) => {
                write!(f, "$\"{}\"", self.interner.lookup(*id).unwrap_or(""))
            }
            TokenKind::TemplateHead(id) => {
                write!(f, "$\"{}", self.interner.lookup(*id).unwrap_or(""))
            }
            TokenKind::TemplateMiddle(id) => {
                write!(f, "{}", self.interner.lookup(*id).unwrap_or(""))
            }
            TokenKind::TemplateTail(id) => {
                write!(f, "{}\"", self.interner.lookup(*id).unwrap_or(""))
            }
            TokenKind::Invalid(id) => {
                write!(f, "{}", self.interner.lookup(*id).unwrap_or("<invalid>"))
            }
            kind => {
                if let Some((kw, _)) = KEYWORDS.iter().find(|(_, tk)| *tk == *kind) {
                    return write!(f, "{kw}");
                }
                if let Some((_, sym)) = SYMBOLS.iter().find(|(tk, _)| *tk == *kind) {
                    return write!(f, "{sym}");
                }
                write!(f, "<unknown>")
            }
        }
    }
}
