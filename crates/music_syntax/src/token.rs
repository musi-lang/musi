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

    // Template literal tokens.
    //
    // Note: These are *chunk* tokens that include their boundary markers in the
    // token text (e.g. head starts with '`' and ends with '${').
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
    KwForall,
    KwHandle,
    KwIf,
    KwImport,
    KwIn,
    KwInfix,
    KwInfixl,
    KwInfixr,
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
    KwUsing,
    KwWhere,
    KwXor,

    // Prefixes (grammar/Musi.abnf)
    At,
    Hash,
    Backslash,

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
    MinusGt,         // ->
    TildeGt,         // ~>
    EqGt,            // =>
    SlashEq,         // /=
    LtEq,            // <=
    GtEq,            // >=
    LtColon,         // <:
    DotDot,          // ..
    DotDotLt,        // ..<
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
    (b"..<", TokenKind::DotDotLt),
    (b":?>", TokenKind::ColonQuestionGt),
    (b":=", TokenKind::ColonEq),
    (b"...", TokenKind::DotDotDot),
    (b"..", TokenKind::DotDot),
    (b".{", TokenKind::DotLBrace),
    (b".[", TokenKind::DotLBracket),
    (b"=>", TokenKind::EqGt),
    (b"->", TokenKind::MinusGt),
    (b"/=", TokenKind::SlashEq),
    (b"<=", TokenKind::LtEq),
    (b"<:", TokenKind::LtColon),
    (b">=", TokenKind::GtEq),
    (b"|>", TokenKind::PipeGt),
    (b"~>", TokenKind::TildeGt),
    (b":?", TokenKind::ColonQuestion),
    (b"#", TokenKind::Hash),
    (b"\\", TokenKind::Backslash),
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

const KEYWORD_NAMES: [(&str, TokenKind, &str); 33] = [
    ("and", TokenKind::KwAnd, "`and`"),
    ("as", TokenKind::KwAs, "`as`"),
    ("case", TokenKind::KwCase, "`case`"),
    ("class", TokenKind::KwClass, "`class`"),
    ("data", TokenKind::KwData, "`data`"),
    ("effect", TokenKind::KwEffect, "`effect`"),
    ("export", TokenKind::KwExport, "`export`"),
    ("foreign", TokenKind::KwForeign, "`foreign`"),
    ("forall", TokenKind::KwForall, "`forall`"),
    ("handle", TokenKind::KwHandle, "`handle`"),
    ("if", TokenKind::KwIf, "`if`"),
    ("import", TokenKind::KwImport, "`import`"),
    ("in", TokenKind::KwIn, "`in`"),
    ("infix", TokenKind::KwInfix, "`infix`"),
    ("infixl", TokenKind::KwInfixl, "`infixl`"),
    ("infixr", TokenKind::KwInfixr, "`infixr`"),
    ("instance", TokenKind::KwInstance, "`instance`"),
    ("law", TokenKind::KwLaw, "`law`"),
    ("let", TokenKind::KwLet, "`let`"),
    ("mut", TokenKind::KwMut, "`mut`"),
    ("perform", TokenKind::KwPerform, "`perform`"),
    ("not", TokenKind::KwNot, "`not`"),
    ("of", TokenKind::KwOf, "`of`"),
    ("opaque", TokenKind::KwOpaque, "`opaque`"),
    ("or", TokenKind::KwOr, "`or`"),
    ("quote", TokenKind::KwQuote, "`quote`"),
    ("rec", TokenKind::KwRec, "`rec`"),
    ("resume", TokenKind::KwResume, "`resume`"),
    ("shl", TokenKind::KwShl, "`shl`"),
    ("shr", TokenKind::KwShr, "`shr`"),
    ("using", TokenKind::KwUsing, "`using`"),
    ("where", TokenKind::KwWhere, "`where`"),
    ("xor", TokenKind::KwXor, "`xor`"),
];

const PUNCT_DISPLAY: [(TokenKind, &str); 39] = [
    (TokenKind::At, "`@`"),
    (TokenKind::Hash, "`#`"),
    (TokenKind::Backslash, "`\\\\`"),
    (TokenKind::LParen, "`(`"),
    (TokenKind::RParen, "`)`"),
    (TokenKind::LBracket, "`[`"),
    (TokenKind::RBracket, "`]`"),
    (TokenKind::LBrace, "`{`"),
    (TokenKind::RBrace, "`}`"),
    (TokenKind::Comma, "`,`"),
    (TokenKind::Semicolon, "`;`"),
    (TokenKind::Dot, "`.`"),
    (TokenKind::Colon, "`:`"),
    (TokenKind::Pipe, "`|`"),
    (TokenKind::Underscore, "`_`"),
    (TokenKind::Plus, "`+`"),
    (TokenKind::Minus, "`-`"),
    (TokenKind::Star, "`*`"),
    (TokenKind::Slash, "`/`"),
    (TokenKind::Percent, "`%`"),
    (TokenKind::Eq, "`=`"),
    (TokenKind::Lt, "`<`"),
    (TokenKind::Gt, "`>`"),
    (TokenKind::ColonEq, "`:=`"),
    (TokenKind::MinusGt, "`->`"),
    (TokenKind::TildeGt, "`~>`"),
    (TokenKind::EqGt, "`=>`"),
    (TokenKind::SlashEq, "`/=`"),
    (TokenKind::LtEq, "`<=`"),
    (TokenKind::GtEq, "`>=`"),
    (TokenKind::LtColon, "`<:`"),
    (TokenKind::DotDot, "`..`"),
    (TokenKind::DotDotLt, "`..<`"),
    (TokenKind::DotDotDot, "`...`"),
    (TokenKind::DotLBrace, "`.{`"),
    (TokenKind::DotLBracket, "`.[`"),
    (TokenKind::ColonQuestion, "`:?`"),
    (TokenKind::ColonQuestionGt, "`:?>`"),
    (TokenKind::PipeGt, "`|>`"),
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
        KEYWORD_NAMES
            .iter()
            .find_map(|(name, kind, _)| if *name == s { Some(*kind) } else { None })
    }

    #[must_use]
    pub fn is_keyword(self) -> bool {
        self.keyword_display().is_some()
    }

    fn keyword_display(self) -> Option<&'static str> {
        KEYWORD_NAMES
            .iter()
            .find_map(|(_, kind, display)| if *kind == self { Some(*display) } else { None })
    }

    fn punct_display(self) -> Option<&'static str> {
        PUNCT_DISPLAY
            .iter()
            .find_map(|(kind, display)| if *kind == self { Some(*display) } else { None })
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", display_token_kind(*self))
    }
}

#[must_use]
pub fn display_token_kind(kind: TokenKind) -> &'static str {
    if let Some(display) = kind.keyword_display() {
        return display;
    }
    if let Some(display) = kind.punct_display() {
        return display;
    }
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
        TokenKind::SymbolicOp => "symbolic operator",
        _ => "token",
    }
}
