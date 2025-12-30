use musi_basic::interner::Interner;
use musi_basic::span::Span;
use musi_basic::types::Ident;
use std::borrow::Cow;
use std::fmt;
use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumericBase {
    Decimal,
    Hex,
    Octal,
    Binary,
}

impl NumericBase {
    #[must_use]
    pub const fn radix(&self) -> u32 {
        match self {
            Self::Decimal => 10,
            Self::Hex => 16,
            Self::Octal => 8,
            Self::Binary => 2,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumericSuffix {
    I8,
    I16,
    I32,
    I64,
    N8,
    N16,
    N32,
    N64,
    F16,
    F32,
    F64,
}

impl FromStr for NumericSuffix {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.trim_start_matches('_') {
            "i8" => Ok(Self::I8),
            "i16" => Ok(Self::I16),
            "i32" => Ok(Self::I32),
            "i64" => Ok(Self::I64),
            "n8" => Ok(Self::N8),
            "n16" => Ok(Self::N16),
            "n32" => Ok(Self::N32),
            "n64" => Ok(Self::N64),
            "f16" => Ok(Self::F16),
            "f32" => Ok(Self::F32),
            "f64" => Ok(Self::F64),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[non_exhaustive]
pub enum TokenKind {
    Ident(Ident),

    // Literals
    LitInt {
        raw: Ident,
        base: NumericBase,
        suffix: Option<NumericSuffix>,
    },
    LitReal {
        raw: Ident,
        suffix: Option<NumericSuffix>,
    },
    LitString(Ident),
    LitRune(char),
    LitTemplateNoSubst(Ident),
    TemplateHead(Ident),
    TemplateMiddle(Ident),
    TemplateTail(Ident),

    // Trivia
    Whitespace,
    Newline,
    LineComment {
        docstyle: bool,
    },
    BlockComment {
        docstyle: bool,
    },

    // Keywords (alphabetically)
    KwAnd,
    KwAs,
    KwBreak,
    KwCase,
    KwChoice,
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
    KwNot,
    KwOr,
    KwRecord,
    KwReturn,
    KwTrue,
    KwType,
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
    Error(Ident),
    EOF,
}

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub const DUMMY: Self = Self {
        kind: TokenKind::EOF,
        span: Span::DUMMY,
    };

    #[must_use]
    pub const fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

pub const KEYWORDS: &[(&str, TokenKind)] = &[
    ("and", TokenKind::KwAnd),
    ("as", TokenKind::KwAs),
    ("break", TokenKind::KwBreak),
    ("case", TokenKind::KwCase),
    ("choice", TokenKind::KwChoice),
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
    ("not", TokenKind::KwNot),
    ("or", TokenKind::KwOr),
    ("record", TokenKind::KwRecord),
    ("return", TokenKind::KwReturn),
    ("true", TokenKind::KwTrue),
    ("type", TokenKind::KwType),
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
            TokenKind::LitInt { raw, suffix, .. } | TokenKind::LitReal { raw, suffix } => {
                let s = self.interner.lookup(raw.id).unwrap_or("");
                write!(f, "{s}")?;
                if let Some(suf) = suffix {
                    write!(f, "{suf:?}")?;
                }
                Ok(())
            }
            TokenKind::LitString(ident) => {
                write!(f, "\"{}\"", self.interner.lookup(ident.id).unwrap_or(""))
            }
            TokenKind::LitRune(c) => write!(f, "'{c}'"),
            TokenKind::LitTemplateNoSubst(ident) => {
                write!(f, "$\"{}\"", self.interner.lookup(ident.id).unwrap_or(""))
            }
            TokenKind::TemplateHead(ident) => {
                write!(f, "$\"{}", self.interner.lookup(ident.id).unwrap_or(""))
            }
            TokenKind::TemplateMiddle(ident) => {
                write!(f, "{}", self.interner.lookup(ident.id).unwrap_or(""))
            }
            TokenKind::TemplateTail(ident) => {
                write!(f, "{}\"", self.interner.lookup(ident.id).unwrap_or(""))
            }
            TokenKind::Ident(ident) | TokenKind::Error(ident) => {
                write!(f, "{}", self.interner.resolve(ident.id))
            }
            TokenKind::Whitespace => write!(f, " "),
            TokenKind::Newline => write!(f, "\\n"),
            TokenKind::LineComment { .. } => write!(f, "//..."),
            TokenKind::BlockComment { .. } => write!(f, "/*...*/"),
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
