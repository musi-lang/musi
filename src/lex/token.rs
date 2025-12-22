use crate::basic::interner::Interner;
use crate::basic::span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    LBrackLt,
    GtRBrack,
    LParen,
    RParen,
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
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub const fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

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
    ("try", TokenKind::KwTry),
    ("unsafe", TokenKind::KwUnsafe),
    ("val", TokenKind::KwVal),
    ("var", TokenKind::KwVar),
    ("while", TokenKind::KwWhile),
    ("with", TokenKind::KwWith),
];

pub struct TokenDisplay<'a> {
    kind: &'a TokenKind,
    interner: &'a Interner,
}

impl TokenKind {
    pub fn display<'a>(&'a self, interner: &'a Interner) -> TokenDisplay<'a> {
        TokenDisplay {
            kind: self,
            interner,
        }
    }
}

impl<'a> std::fmt::Display for TokenDisplay<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            TokenKind::Ident(id) => write!(f, "{}", self.interner.lookup(*id).unwrap_or("<ident>")),
            TokenKind::LitInt(id) => write!(f, "{}", self.interner.lookup(*id).unwrap_or("<int>")),
            TokenKind::LitReal(id) => {
                write!(f, "{}", self.interner.lookup(*id).unwrap_or("<real>"))
            }
            TokenKind::LitString(id) => {
                write!(f, "\"{}\"", self.interner.lookup(*id).unwrap_or(""))
            }
            TokenKind::LitRune(c) => write!(f, "'{}'", c),
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
            TokenKind::KwAlias => write!(f, "alias"),
            TokenKind::KwAnd => write!(f, "and"),
            TokenKind::KwAs => write!(f, "as"),
            TokenKind::KwBreak => write!(f, "break"),
            TokenKind::KwCase => write!(f, "case"),
            TokenKind::KwCycle => write!(f, "cycle"),
            TokenKind::KwDefer => write!(f, "defer"),
            TokenKind::KwElse => write!(f, "else"),
            TokenKind::KwExtern => write!(f, "extern"),
            TokenKind::KwFalse => write!(f, "false"),
            TokenKind::KwFn => write!(f, "fn"),
            TokenKind::KwFor => write!(f, "for"),
            TokenKind::KwIf => write!(f, "if"),
            TokenKind::KwImport => write!(f, "import"),
            TokenKind::KwIn => write!(f, "in"),
            TokenKind::KwIs => write!(f, "is"),
            TokenKind::KwMatch => write!(f, "match"),
            TokenKind::KwMod => write!(f, "mod"),
            TokenKind::KwNot => write!(f, "not"),
            TokenKind::KwOr => write!(f, "or"),
            TokenKind::KwRecord => write!(f, "record"),
            TokenKind::KwReturn => write!(f, "return"),
            TokenKind::KwSum => write!(f, "sum"),
            TokenKind::KwTrue => write!(f, "true"),
            TokenKind::KwTry => write!(f, "try"),
            TokenKind::KwUnsafe => write!(f, "unsafe"),
            TokenKind::KwVal => write!(f, "val"),
            TokenKind::KwVar => write!(f, "var"),
            TokenKind::KwWhile => write!(f, "while"),
            TokenKind::KwWith => write!(f, "with"),
            TokenKind::LBrace => write!(f, "{{"),
            TokenKind::RBrace => write!(f, "}}"),
            TokenKind::LBrack => write!(f, "["),
            TokenKind::RBrack => write!(f, "]"),
            TokenKind::LBrackLt => write!(f, "[<"),
            TokenKind::GtRBrack => write!(f, ">]"),
            TokenKind::LParen => write!(f, "("),
            TokenKind::RParen => write!(f, ")"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Eq => write!(f, "="),
            TokenKind::SlashEq => write!(f, "/="),
            TokenKind::Lt => write!(f, "<"),
            TokenKind::LtEq => write!(f, "<="),
            TokenKind::Gt => write!(f, ">"),
            TokenKind::GtEq => write!(f, ">="),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Percent => write!(f, "%"),
            TokenKind::LtLt => write!(f, "<<"),
            TokenKind::GtGt => write!(f, ">>"),
            TokenKind::StarStar => write!(f, "**"),
            TokenKind::Amp => write!(f, "&"),
            TokenKind::Bar => write!(f, "|"),
            TokenKind::BarGt => write!(f, "|>"),
            TokenKind::Caret => write!(f, "^"),
            TokenKind::Tilde => write!(f, "~"),
            TokenKind::At => write!(f, "@"),
            TokenKind::DotCaret => write!(f, ".^"),
            TokenKind::ColonColon => write!(f, "::"),
            TokenKind::QuestionQuestion => write!(f, "??"),
            TokenKind::DotDot => write!(f, ".."),
            TokenKind::DotDotLt => write!(f, "..<"),
            TokenKind::MinusGt => write!(f, "->"),
            TokenKind::LtMinus => write!(f, "<-"),
            TokenKind::ColonEq => write!(f, ":="),
            TokenKind::EqGt => write!(f, "=>"),
            TokenKind::Question => write!(f, "?"),
            TokenKind::Underscore => write!(f, "_"),
            TokenKind::Dollar => write!(f, "$"),
            TokenKind::EOF => write!(f, "EOF"),
            TokenKind::Invalid(id) => {
                write!(f, "{}", self.interner.lookup(*id).unwrap_or("<invalid>"))
            }
        }
    }
}
