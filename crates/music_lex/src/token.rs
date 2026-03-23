use music_found::Span;

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
    KwChoice,
    KwClass,
    KwDefer,
    KwEffect,
    KwExport,
    KwFatal,
    KwForeign,
    KwHandle,
    KwIf,
    KwImport,
    KwIn,
    KwInstance,
    KwLaw,
    KwLet,
    KwMatch,
    KwMut,
    KwNeed,
    KwNot,
    KwOf,
    KwOpaque,
    KwOr,
    KwQuote,
    KwRecord,
    KwResume,
    KwReturn,
    KwTry,
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
    Dollar,
    DollarLParen,
    DollarLBracket,

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
        "choice" => Some(TokenKind::KwChoice),
        "class" => Some(TokenKind::KwClass),
        "defer" => Some(TokenKind::KwDefer),
        "effect" => Some(TokenKind::KwEffect),
        "export" => Some(TokenKind::KwExport),
        "fatal" => Some(TokenKind::KwFatal),
        "foreign" => Some(TokenKind::KwForeign),
        "handle" => Some(TokenKind::KwHandle),
        "if" => Some(TokenKind::KwIf),
        "import" => Some(TokenKind::KwImport),
        "in" => Some(TokenKind::KwIn),
        "instance" => Some(TokenKind::KwInstance),
        "law" => Some(TokenKind::KwLaw),
        "let" => Some(TokenKind::KwLet),
        "match" => Some(TokenKind::KwMatch),
        "mut" => Some(TokenKind::KwMut),
        "need" => Some(TokenKind::KwNeed),
        "not" => Some(TokenKind::KwNot),
        "of" => Some(TokenKind::KwOf),
        "opaque" => Some(TokenKind::KwOpaque),
        "or" => Some(TokenKind::KwOr),
        "quote" => Some(TokenKind::KwQuote),
        "record" => Some(TokenKind::KwRecord),
        "resume" => Some(TokenKind::KwResume),
        "return" => Some(TokenKind::KwReturn),
        "try" => Some(TokenKind::KwTry),
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
        '$' => Some(TokenKind::Dollar),
        _ => None,
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
