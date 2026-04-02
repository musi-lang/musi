use music_base::Span;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Eof,
    Error,

    Ident,
    OpIdent,

    Int,
    Float,
    String,
    Rune,

    TemplateNoSubst,
    TemplateHead,
    TemplateMiddle,
    TemplateTail,

    // Keywords (grammar/Musi.abnf)
    KwAnd,
    KwAs,
    KwCase,
    KwClass,
    KwData,
    KwEffect,
    KwExport,
    KwForeign,
    KwHandle,
    KwIf,
    KwImport,
    KwIn,
    KwInstance,
    KwLaw,
    KwLet,
    KwMut,
    KwPerform,
    KwNot,
    KwOf,
    KwOpaque,
    KwOr,
    KwQuote,
    KwRec,
    KwResume,
    KwShl,
    KwShr,
    KwWhere,
    KwWith,
    KwXor,

    // Prefixes (grammar/Musi.abnf)
    At,
    Hash,

    // Separators / punctuation.
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Comma,
    Semicolon,

    // Operators / punctuation.
    Dot,
    Colon,
    Pipe,
    Underscore,

    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Eq,
    Lt,
    Gt,

    // Compound tokens (grammar/Musi.abnf)
    ColonEq,         // :=
    LtMinus,         // <-
    MinusGt,         // ->
    TildeGt,         // ~>
    EqGt,            // =>
    SlashEq,         // /=
    LtEq,            // <=
    GtEq,            // >=
    LtColon,         // <:
    QDot,            // ?.
    BangDot,         // !.
    DotDotDot,       // ...
    DotLBrace,       // .{
    DotLBracket,     // .[
    ColonQuestion,   // :?
    ColonQuestionGt, // :?>
    PipeGt,          // |>

    // User-defined symbolic operator token (2+ sym-char).
    SymbolicOp,
}

// Ordered by longest-to-shortest (maximal munch).
pub const TOKEN_PATTERNS: &[(&[u8], TokenKind)] = &[
    (b":?>", TokenKind::ColonQuestionGt),
    (b"...", TokenKind::DotDotDot),
    (b".{", TokenKind::DotLBrace),
    (b".[", TokenKind::DotLBracket),
    (b"=>", TokenKind::EqGt),
    (b"!.", TokenKind::BangDot),
    (b"->", TokenKind::MinusGt),
    (b"/=", TokenKind::SlashEq),
    (b"<=", TokenKind::LtEq),
    (b"<:", TokenKind::LtColon),
    (b"<-", TokenKind::LtMinus),
    (b">=", TokenKind::GtEq),
    (b"?.", TokenKind::QDot),
    (b"|>", TokenKind::PipeGt),
    (b"~>", TokenKind::TildeGt),
    (b":=", TokenKind::ColonEq),
    (b":?", TokenKind::ColonQuestion),
    (b"#", TokenKind::Hash),
    (b"%", TokenKind::Percent),
    (b"*", TokenKind::Star),
    (b"+", TokenKind::Plus),
    (b",", TokenKind::Comma),
    (b"-", TokenKind::Minus),
    (b".", TokenKind::Dot),
    (b"/", TokenKind::Slash),
    (b":", TokenKind::Colon),
    (b";", TokenKind::Semicolon),
    (b"<", TokenKind::Lt),
    (b"=", TokenKind::Eq),
    (b">", TokenKind::Gt),
    (b"@", TokenKind::At),
    (b"[", TokenKind::LBracket),
    (b"]", TokenKind::RBracket),
    (b"{", TokenKind::LBrace),
    (b"|", TokenKind::Pipe),
    (b"}", TokenKind::RBrace),
    (b"(", TokenKind::LParen),
    (b")", TokenKind::RParen),
    (b"_", TokenKind::Underscore),
];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    #[must_use]
    pub const fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl TokenKind {
    #[must_use]
    pub(crate) fn keyword_from_str(s: &str) -> Option<Self> {
        Some(match s {
            "and" => Self::KwAnd,
            "as" => Self::KwAs,
            "case" => Self::KwCase,
            "class" => Self::KwClass,
            "data" => Self::KwData,
            "effect" => Self::KwEffect,
            "export" => Self::KwExport,
            "foreign" => Self::KwForeign,
            "handle" => Self::KwHandle,
            "if" => Self::KwIf,
            "import" => Self::KwImport,
            "in" => Self::KwIn,
            "instance" => Self::KwInstance,
            "law" => Self::KwLaw,
            "let" => Self::KwLet,
            "mut" => Self::KwMut,
            "perform" => Self::KwPerform,
            "not" => Self::KwNot,
            "of" => Self::KwOf,
            "opaque" => Self::KwOpaque,
            "or" => Self::KwOr,
            "quote" => Self::KwQuote,
            "rec" => Self::KwRec,
            "resume" => Self::KwResume,
            "shl" => Self::KwShl,
            "shr" => Self::KwShr,
            "where" => Self::KwWhere,
            "with" => Self::KwWith,
            "xor" => Self::KwXor,
            _ => return None,
        })
    }

    #[must_use]
    pub const fn is_keyword(self) -> bool {
        matches!(
            self,
            Self::KwAnd
                | Self::KwAs
                | Self::KwCase
                | Self::KwClass
                | Self::KwData
                | Self::KwEffect
                | Self::KwExport
                | Self::KwForeign
                | Self::KwHandle
                | Self::KwIf
                | Self::KwImport
                | Self::KwIn
                | Self::KwInstance
                | Self::KwLaw
                | Self::KwLet
                | Self::KwMut
                | Self::KwPerform
                | Self::KwNot
                | Self::KwOf
                | Self::KwOpaque
                | Self::KwOr
                | Self::KwQuote
                | Self::KwRec
                | Self::KwResume
                | Self::KwShl
                | Self::KwShr
                | Self::KwWhere
                | Self::KwWith
                | Self::KwXor
        )
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", display_token_kind(*self))
    }
}

#[must_use]
pub const fn display_token_kind(kind: TokenKind) -> &'static str {
    match kind {
        TokenKind::Eof => "end of file",
        TokenKind::Error => "invalid token",
        TokenKind::Ident => "identifier",
        TokenKind::OpIdent => "operator identifier",
        TokenKind::Int => "integer literal",
        TokenKind::Float => "float literal",
        TokenKind::String => "string literal",
        TokenKind::Rune => "rune literal",
        TokenKind::TemplateNoSubst => "template literal",
        TokenKind::TemplateHead => "template head",
        TokenKind::TemplateMiddle => "template middle",
        TokenKind::TemplateTail => "template tail",
        TokenKind::KwAnd => "`and`",
        TokenKind::KwAs => "`as`",
        TokenKind::KwCase => "`case`",
        TokenKind::KwClass => "`class`",
        TokenKind::KwData => "`data`",
        TokenKind::KwEffect => "`effect`",
        TokenKind::KwExport => "`export`",
        TokenKind::KwForeign => "`foreign`",
        TokenKind::KwHandle => "`handle`",
        TokenKind::KwIf => "`if`",
        TokenKind::KwImport => "`import`",
        TokenKind::KwIn => "`in`",
        TokenKind::KwInstance => "`instance`",
        TokenKind::KwLaw => "`law`",
        TokenKind::KwLet => "`let`",
        TokenKind::KwMut => "`mut`",
        TokenKind::KwPerform => "`perform`",
        TokenKind::KwNot => "`not`",
        TokenKind::KwOf => "`of`",
        TokenKind::KwOpaque => "`opaque`",
        TokenKind::KwOr => "`or`",
        TokenKind::KwQuote => "`quote`",
        TokenKind::KwRec => "`rec`",
        TokenKind::KwResume => "`resume`",
        TokenKind::KwShl => "`shl`",
        TokenKind::KwShr => "`shr`",
        TokenKind::KwWhere => "`where`",
        TokenKind::KwWith => "`with`",
        TokenKind::KwXor => "`xor`",
        TokenKind::At => "`@`",
        TokenKind::Hash => "`#`",
        TokenKind::LParen => "`(`",
        TokenKind::RParen => "`)`",
        TokenKind::LBracket => "`[`",
        TokenKind::RBracket => "`]`",
        TokenKind::LBrace => "`{`",
        TokenKind::RBrace => "`}`",
        TokenKind::Comma => "`,`",
        TokenKind::Semicolon => "`;`",
        TokenKind::Dot => "`.`",
        TokenKind::Colon => "`:`",
        TokenKind::Pipe => "`|`",
        TokenKind::Underscore => "`_`",
        TokenKind::Plus => "`+`",
        TokenKind::Minus => "`-`",
        TokenKind::Star => "`*`",
        TokenKind::Slash => "`/`",
        TokenKind::Percent => "`%`",
        TokenKind::Eq => "`=`",
        TokenKind::Lt => "`<`",
        TokenKind::Gt => "`>`",
        TokenKind::ColonEq => "`:=`",
        TokenKind::LtMinus => "`<-`",
        TokenKind::MinusGt => "`->`",
        TokenKind::TildeGt => "`~>`",
        TokenKind::EqGt => "`=>`",
        TokenKind::SlashEq => "`/=`",
        TokenKind::LtEq => "`<=`",
        TokenKind::GtEq => "`>=`",
        TokenKind::LtColon => "`<:`",
        TokenKind::QDot => "`?.`",
        TokenKind::BangDot => "`!.`",
        TokenKind::DotDotDot => "`...`",
        TokenKind::DotLBrace => "`.{`",
        TokenKind::DotLBracket => "`.[`",
        TokenKind::ColonQuestion => "`:?`",
        TokenKind::ColonQuestionGt => "`:?>`",
        TokenKind::PipeGt => "`|>`",
        TokenKind::SymbolicOp => "symbolic operator",
    }
}
