use music_shared::Span;

pub type TriviaList = Vec<Trivia>;

#[derive(Debug, Clone, PartialEq)]
pub enum FStrPart {
    Lit(String),
    Expr(Vec<Token>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub leading_trivia: TriviaList,
    pub trailing_trivia: TriviaList,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Trivia {
    pub kind: TriviaKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TriviaKind {
    Whitespace,
    LineComment { doc: bool },
    BlockComment { doc: bool },
    Newline,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Literals
    Int(i64),
    Float(f64),
    Str(String),
    FStr(Vec<FStrPart>),
    Rune(char),

    // Identifiers
    Ident,
    EscapedIdent,

    // Keywords
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
    KwNeed,
    KwNot,
    KwOf,
    KwOpaque,
    KwOr,
    KwQuote,
    KwResume,
    KwReturn,
    KwVia,
    KwWhere,
    KwWith,
    KwXor,

    // Delimiters
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,

    // Punctuation
    Semi,
    Comma,
    Dot,
    Colon,
    Pipe,
    Bang,
    Question,
    Lt,
    Gt,
    Eq,

    // Compound
    ColonEq,
    LtMinus,
    MinusGt,
    TildeGt,
    EqGt,
    SlashEq,
    LtEq,
    GtEq,
    DotDot,
    DotDotLt,
    DotDotDot,
    DotLBracket,
    DotLBrace,
    ColonQuestion,
    ColonQuestionGt,
    LtColon,
    PipeGt,
    ColonColon,
    QuestionDot,
    BangDot,
    QuestionQuestion,
    At,
    Hash,
    HashLParen,
    HashLBracket,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,

    // Special
    Eof,
}

#[must_use]
pub fn keyword_from_str(s: &str) -> Option<TokenKind> {
    match s {
        "and" => Some(TokenKind::KwAnd),
        "as" => Some(TokenKind::KwAs),
        "case" => Some(TokenKind::KwCase),
        "class" => Some(TokenKind::KwClass),
        "data" => Some(TokenKind::KwData),
        "effect" => Some(TokenKind::KwEffect),
        "export" => Some(TokenKind::KwExport),
        "foreign" => Some(TokenKind::KwForeign),
        "handle" => Some(TokenKind::KwHandle),
        "if" => Some(TokenKind::KwIf),
        "import" => Some(TokenKind::KwImport),
        "in" => Some(TokenKind::KwIn),
        "instance" => Some(TokenKind::KwInstance),
        "law" => Some(TokenKind::KwLaw),
        "let" => Some(TokenKind::KwLet),
        "mut" => Some(TokenKind::KwMut),
        "need" => Some(TokenKind::KwNeed),
        "not" => Some(TokenKind::KwNot),
        "of" => Some(TokenKind::KwOf),
        "opaque" => Some(TokenKind::KwOpaque),
        "or" => Some(TokenKind::KwOr),
        "quote" => Some(TokenKind::KwQuote),
        "resume" => Some(TokenKind::KwResume),
        "return" => Some(TokenKind::KwReturn),
        "via" => Some(TokenKind::KwVia),
        "where" => Some(TokenKind::KwWhere),
        "with" => Some(TokenKind::KwWith),
        "xor" => Some(TokenKind::KwXor),
        _ => None,
    }
}

#[must_use]
pub const fn single_char_token(ch: char) -> Option<TokenKind> {
    match ch {
        '(' => Some(TokenKind::LParen),
        ')' => Some(TokenKind::RParen),
        '[' => Some(TokenKind::LBracket),
        ']' => Some(TokenKind::RBracket),
        '{' => Some(TokenKind::LBrace),
        '}' => Some(TokenKind::RBrace),
        ';' => Some(TokenKind::Semi),
        ',' => Some(TokenKind::Comma),
        '.' => Some(TokenKind::Dot),
        ':' => Some(TokenKind::Colon),
        '|' => Some(TokenKind::Pipe),
        '!' => Some(TokenKind::Bang),
        '?' => Some(TokenKind::Question),
        '<' => Some(TokenKind::Lt),
        '>' => Some(TokenKind::Gt),
        '=' => Some(TokenKind::Eq),
        '+' => Some(TokenKind::Plus),
        '-' => Some(TokenKind::Minus),
        '*' => Some(TokenKind::Star),
        '/' => Some(TokenKind::Slash),
        '%' => Some(TokenKind::Percent),
        '@' => Some(TokenKind::At),
        '#' => Some(TokenKind::Hash),
        _ => None,
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
