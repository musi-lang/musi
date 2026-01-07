use crate::intern::Interner;
use crate::span::Span;
use crate::symbol::Symbol;
use std::borrow::Cow;
use std::fmt;
use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Numeric base for integer literals.
pub enum NumericBase {
    /// Base 10.
    Decimal,
    /// Base 16.
    Hex,
    /// Base 8.
    Octal,
    /// Base 2.
    Binary,
}

impl NumericBase {
    #[must_use]
    /// Returns radix value.
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
/// Suffix for numeric literals.
pub enum NumericSuffix {
    /// Signed 8-bit integer.
    I8,
    /// Signed 16-bit integer.
    I16,
    /// Signed 32-bit integer.
    I32,
    /// Signed 64-bit integer.
    I64,
    /// Unsigned 8-bit integer.
    N8,
    /// Unsigned 16-bit integer.
    N16,
    /// Unsigned 32-bit integer.
    N32,
    /// Unsigned 64-bit integer.
    N64,
    /// Binary 16-bit floating-point.
    F16,
    /// Binary 32-bit floating-point.
    F32,
    /// Binary 64-bit floating-point.
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
/// Kind of lexical token.
pub enum TokenKind {
    /// `variable_name`, `foo`
    Ident(Symbol),

    /// `42`, `0xFF`, `0b1010`
    LitInt {
        raw: Symbol,
        base: NumericBase,
        suffix: Option<NumericSuffix>,
    },
    /// `3.14`, `1.5e-10`
    LitFloat {
        raw: Symbol,
        suffix: Option<NumericSuffix>,
    },
    /// `"hello world"`
    LitString(Symbol),
    /// `'a'`, `'\n'`
    LitRune(char),
    /// `$\"text\"`
    LitTemplateNoSubst(Symbol),
    /// `$\"Head {`
    TemplateHead(Symbol),
    /// `} Middle {`
    TemplateMiddle(Symbol),
    /// `} Tail\"`
    TemplateTail(Symbol),

    /// ` `, `\t`
    Whitespace,
    /// `\n` or `\r\n`
    Newline,
    /// `// comment`
    LineComment {
        /// `/// comment`
        docstyle: bool,
    },
    /// `/* comment */`
    BlockComment {
        /// `/** comment */`
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
    /// `{`
    LBrace,
    /// `}`
    RBrace,
    /// `[`
    LBrack,
    /// `]`
    RBrack,
    /// `!`
    Bang,
    /// `(`
    LParen,
    /// `)`
    RParen,
    /// `@[`
    AtLBrack,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `:`
    Colon,
    /// `;`
    Semicolon,
    /// `=`
    Eq,
    /// `/=`
    SlashEq,
    /// `<`
    Lt,
    /// `<=`
    LtEq,
    /// `>`
    Gt,
    /// `>=`
    GtEq,
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `%`
    Percent,
    /// `<<`
    LtLt,
    /// `>>`
    GtGt,
    /// `**`
    StarStar,
    /// `&`
    Amp,
    /// `|`
    Bar,
    /// `|>`
    BarGt,
    /// `^`
    Caret,
    /// `~`
    Tilde,
    /// `@`
    At,
    /// `.^`
    DotCaret,
    /// `::`
    ColonColon,
    /// `??`
    QuestionQuestion,
    /// `..`
    DotDot,
    /// `..<`
    DotDotLt,
    /// `->`
    MinusGt,
    /// `<-`
    LtMinus,
    /// `:=`
    ColonEq,
    /// `=>`
    EqGt,
    /// `?`
    Question,
    /// `_`
    Underscore,
    /// `$`
    Dollar,

    // Special
    Error(Symbol),
    EOF,
}

#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
/// Lexical token with span.
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    #[must_use]
    /// Creates new token.
    pub const fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    /// Dummy token for testing/placeholders.
    pub const DUMMY: Self = Self::new(
        TokenKind::Error(Symbol::new(0, Span::new(0, 0))),
        Span::DUMMY,
    );
}

/// List of language keywords.
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

/// List of language symbols.
pub const SYMBOLS: &[(TokenKind, &str)] = &[
    (TokenKind::LBrace, "{"),
    (TokenKind::RBrace, "}"),
    (TokenKind::LBrack, "["),
    (TokenKind::RBrack, "]"),
    (TokenKind::Bang, "!"),
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
/// Helper for displaying token kind with interned string.
pub struct TokenDisplay<'a> {
    kind: &'a TokenKind,
    interner: &'a Interner,
}

impl TokenKind {
    #[must_use]
    /// Creates display helper for token kind.
    pub const fn display<'a>(&'a self, interner: &'a Interner) -> TokenDisplay<'a> {
        TokenDisplay {
            kind: self,
            interner,
        }
    }

    #[must_use]
    /// Returns string representation of token kind if static.
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
            TokenKind::LitInt { raw, suffix, .. } | TokenKind::LitFloat { raw, suffix } => {
                let s = self.interner.lookup(raw.id).unwrap_or("");
                write!(f, "{s}")?;
                if let Some(suf) = suffix {
                    write!(f, "{suf:?}")?;
                }
                Ok(())
            }
            TokenKind::LitString(sym) => {
                write!(f, "\"{}\"", self.interner.lookup(sym.id).unwrap_or(""))
            }
            TokenKind::LitRune(c) => write!(f, "'{c}'"),
            TokenKind::LitTemplateNoSubst(sym) => {
                write!(f, "$\"{}\"", self.interner.lookup(sym.id).unwrap_or(""))
            }
            TokenKind::TemplateHead(sym) => {
                write!(f, "$\"{}", self.interner.lookup(sym.id).unwrap_or(""))
            }
            TokenKind::TemplateMiddle(sym) => {
                write!(f, "{}", self.interner.lookup(sym.id).unwrap_or(""))
            }
            TokenKind::TemplateTail(sym) => {
                write!(f, "{}\"", self.interner.lookup(sym.id).unwrap_or(""))
            }
            TokenKind::Ident(sym) | TokenKind::Error(sym) => {
                write!(f, "{}", self.interner.resolve(sym.id))
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
