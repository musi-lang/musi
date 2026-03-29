use crate::token::TokenKind;

const COMPOUNDS_COLON: &[(&[u8], TokenKind)] = &[
    (b":?>", TokenKind::ColonQuestionGt),
    (b":=", TokenKind::ColonEq),
    (b":?", TokenKind::ColonQuestion),
];
const COMPOUNDS_DOT: &[(&[u8], TokenKind)] = &[
    (b"...", TokenKind::DotDotDot),
    (b".[", TokenKind::DotLBracket),
    (b".{", TokenKind::DotLBrace),
];
const COMPOUNDS_LT: &[(&[u8], TokenKind)] = &[
    (b"<-", TokenKind::LtMinus),
    (b"<=", TokenKind::LtEq),
    (b"<:", TokenKind::LtColon),
];
const COMPOUNDS_MINUS: &[(&[u8], TokenKind)] = &[(b"->", TokenKind::MinusGt)];
const COMPOUNDS_TILDE: &[(&[u8], TokenKind)] = &[(b"~>", TokenKind::TildeGt)];
const COMPOUNDS_EQ: &[(&[u8], TokenKind)] = &[(b"=>", TokenKind::EqGt)];
const COMPOUNDS_SLASH: &[(&[u8], TokenKind)] = &[(b"/=", TokenKind::SlashEq)];
const COMPOUNDS_GT: &[(&[u8], TokenKind)] = &[(b">=", TokenKind::GtEq)];
const COMPOUNDS_QUESTION: &[(&[u8], TokenKind)] = &[(b"?.", TokenKind::QuestionDot)];
const COMPOUNDS_BANG: &[(&[u8], TokenKind)] = &[(b"!.", TokenKind::BangDot)];
const COMPOUNDS_PIPE: &[(&[u8], TokenKind)] = &[(b"|>", TokenKind::PipeGt)];
const COMPOUNDS_HASH: &[(&[u8], TokenKind)] = &[
    (b"#(", TokenKind::SpliceLParen),
    (b"#[", TokenKind::SpliceLBracket),
];

#[must_use]
pub(super) fn keyword_from_text(raw: &str) -> Option<TokenKind> {
    match raw {
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
        "not" => Some(TokenKind::KwNot),
        "of" => Some(TokenKind::KwOf),
        "opaque" => Some(TokenKind::KwOpaque),
        "or" => Some(TokenKind::KwOr),
        "perform" => Some(TokenKind::KwPerform),
        "quote" => Some(TokenKind::KwQuote),
        "resume" => Some(TokenKind::KwResume),
        "shl" => Some(TokenKind::KwShl),
        "shr" => Some(TokenKind::KwShr),
        "where" => Some(TokenKind::KwWhere),
        "with" => Some(TokenKind::KwWith),
        "xor" => Some(TokenKind::KwXor),
        _ => None,
    }
}

#[must_use]
pub(super) const fn single_char_token(ch: char) -> Option<TokenKind> {
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
        '&' => Some(TokenKind::Amp),
        '^' => Some(TokenKind::Caret),
        '~' => Some(TokenKind::Tilde),
        _ => None,
    }
}

#[must_use]
pub(super) fn compound_token(remaining: &[u8]) -> Option<(usize, TokenKind)> {
    let first = *remaining.first()?;
    let table = match first {
        b':' => COMPOUNDS_COLON,
        b'.' => COMPOUNDS_DOT,
        b'<' => COMPOUNDS_LT,
        b'-' => COMPOUNDS_MINUS,
        b'~' => COMPOUNDS_TILDE,
        b'=' => COMPOUNDS_EQ,
        b'/' => COMPOUNDS_SLASH,
        b'>' => COMPOUNDS_GT,
        b'?' => COMPOUNDS_QUESTION,
        b'!' => COMPOUNDS_BANG,
        b'|' => COMPOUNDS_PIPE,
        b'#' => COMPOUNDS_HASH,
        _ => return None,
    };

    for (bytes, kind) in table {
        if remaining.starts_with(bytes) {
            return Some((bytes.len(), kind.clone()));
        }
    }

    None
}
