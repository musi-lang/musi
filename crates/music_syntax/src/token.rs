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
    KwShape,
    KwMatch,
    KwData,
    KwDefer,
    KwElse,
    KwErased,
    KwExport,
    KwHidden,
    KwIf,
    KwImport,
    KwIn,
    KwKnown,
    KwLet,
    KwMut,
    KwNot,
    KwOr,
    KwRecur,
    KwPin,
    KwThen,
    KwUnsafe,
    KwYield,
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
    Question,
    Bang,
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
    ColonEq,          // :=
    ColonGt,          // :>
    ColonQuestionGt,  // :?>
    MinusGt,          // ->
    PipeEq,           // |=
    TildeEq,          // ~=
    EqGt,             // =>
    SlashEq,          // /=
    LtEq,             // <=
    GtEq,             // >=
    DotDot,           // ..
    DotDotLt,         // ..<
    DotDotDot,        // ...
    DotLBracket,      // .[
    DotLParen,        // .(
    QuestionQuestion, // ??
    PipeGt,           // |>

    // User-defined symbolic operator token (2+ sym-char).
    SymbolicOp,
}

// Ordered by longest-to-shortest (maximal munch).
pub const TOKEN_PATTERNS: &[(&[u8], TokenKind)] = &[
    (b":?>", TokenKind::ColonQuestionGt),
    (b":=", TokenKind::ColonEq),
    (b":>", TokenKind::ColonGt),
    (b"...", TokenKind::DotDotDot),
    (b"..<", TokenKind::DotDotLt),
    (b"..", TokenKind::DotDot),
    (b".[", TokenKind::DotLBracket),
    (b".(", TokenKind::DotLParen),
    (b"??", TokenKind::QuestionQuestion),
    (b"=>", TokenKind::EqGt),
    (b"->", TokenKind::MinusGt),
    (b"~=", TokenKind::TildeEq),
    (b"|=", TokenKind::PipeEq),
    (b"/=", TokenKind::SlashEq),
    (b"<=", TokenKind::LtEq),
    (b">=", TokenKind::GtEq),
    (b"|>", TokenKind::PipeGt),
    (b"?", TokenKind::Question),
    (b"!", TokenKind::Bang),
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

const KEYWORD_NAMES: [(&str, TokenKind, &str); 25] = [
    ("and", TokenKind::KwAnd, "`and`"),
    ("as", TokenKind::KwAs, "`as`"),
    ("data", TokenKind::KwData, "`data`"),
    ("defer", TokenKind::KwDefer, "`defer`"),
    ("else", TokenKind::KwElse, "`else`"),
    ("erased", TokenKind::KwErased, "`erased`"),
    ("export", TokenKind::KwExport, "`export`"),
    ("hidden", TokenKind::KwHidden, "`hidden`"),
    ("if", TokenKind::KwIf, "`if`"),
    ("import", TokenKind::KwImport, "`import`"),
    ("in", TokenKind::KwIn, "`in`"),
    ("known", TokenKind::KwKnown, "`known`"),
    ("let", TokenKind::KwLet, "`let`"),
    ("match", TokenKind::KwMatch, "`match`"),
    ("mut", TokenKind::KwMut, "`mut`"),
    ("not", TokenKind::KwNot, "`not`"),
    ("or", TokenKind::KwOr, "`or`"),
    ("pin", TokenKind::KwPin, "`pin`"),
    ("recur", TokenKind::KwRecur, "`recur`"),
    ("shape", TokenKind::KwShape, "`shape`"),
    ("then", TokenKind::KwThen, "`then`"),
    ("unsafe", TokenKind::KwUnsafe, "`unsafe`"),
    ("where", TokenKind::KwWhere, "`where`"),
    ("xor", TokenKind::KwXor, "`xor`"),
    ("yield", TokenKind::KwYield, "`yield`"),
];

const PUNCT_DISPLAY: [(TokenKind, &str); 42] = [
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
    (TokenKind::Question, "`?`"),
    (TokenKind::Bang, "`!`"),
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
    (TokenKind::ColonGt, "`:>`"),
    (TokenKind::ColonQuestionGt, "`:?>`"),
    (TokenKind::MinusGt, "`->`"),
    (TokenKind::PipeEq, "`|=`"),
    (TokenKind::TildeEq, "`~=`"),
    (TokenKind::EqGt, "`=>`"),
    (TokenKind::SlashEq, "`/=`"),
    (TokenKind::LtEq, "`<=`"),
    (TokenKind::GtEq, "`>=`"),
    (TokenKind::DotDot, "`..`"),
    (TokenKind::DotDotLt, "`..<`"),
    (TokenKind::DotDotDot, "`...`"),
    (TokenKind::DotLBracket, "`.[`"),
    (TokenKind::DotLParen, "`.(`"),
    (TokenKind::QuestionQuestion, "`??`"),
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
