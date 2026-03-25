use core::error::Error;
use core::fmt;

use music_found::Span;
use music_lex::TokenKind;

/// A parse error with structured kind, location, and optional context.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
    pub context: Option<&'static str>,
}

/// The specific kind of parse error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseErrorKind {
    ExpectedToken {
        expected: &'static str,
        found: &'static str,
    },
    UnexpectedEof {
        expected: &'static str,
    },
    ExpectedExpr {
        found: &'static str,
    },
    ExpectedPat {
        found: &'static str,
    },
    ExpectedType {
        found: &'static str,
    },
    UnclosedDelimiter {
        open: &'static str,
        open_span: Span,
    },
    InvalidParenForm,
    NonAssociativeChain,
}

impl ParseError {
    /// The primary span for this error.
    #[must_use]
    pub const fn span(&self) -> Span {
        self.span
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.kind, f)?;
        if let Some(ctx) = self.context {
            write!(f, " {ctx}")?;
        }
        Ok(())
    }
}

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ExpectedToken { expected, found } => {
                write!(f, "expected {expected}, found {found}")
            }
            Self::UnexpectedEof { expected } => {
                write!(f, "unexpected end of input, expected {expected}")
            }
            Self::ExpectedExpr { found } => {
                write!(f, "expected expression, found {found}")
            }
            Self::ExpectedPat { found } => {
                write!(f, "expected pattern, found {found}")
            }
            Self::ExpectedType { found } => {
                write!(f, "expected type, found `{found}`")
            }
            Self::UnclosedDelimiter { open, .. } => {
                write!(f, "unclosed delimiter {open}")
            }
            Self::InvalidParenForm => {
                write!(f, "invalid parenthesized form")
            }
            Self::NonAssociativeChain => {
                write!(f, "non-associative operator chained")
            }
        }
    }
}

impl Error for ParseError {}

/// Alias for parser results.
pub type ParseResult<T> = Result<T, ParseError>;

/// Human-readable description of a token kind.
#[must_use]
pub const fn describe_token(kind: &TokenKind) -> &'static str {
    match kind {
        TokenKind::Int(_) => "integer",
        TokenKind::Float(_) => "float",
        TokenKind::Str(_) => "string",
        TokenKind::FStr(_) => "f-string",
        TokenKind::Rune(_) => "rune",
        TokenKind::Ident | TokenKind::EscapedIdent => "identifier",
        TokenKind::KwAnd => "'and'",
        TokenKind::KwAs => "'as'",
        TokenKind::KwChoice => "'choice'",
        TokenKind::KwClass => "'class'",
        TokenKind::KwEffect => "'effect'",
        TokenKind::KwExport => "'export'",
        TokenKind::KwForeign => "'foreign'",
        TokenKind::KwHandle => "'handle'",
        TokenKind::KwIf => "'if'",
        TokenKind::KwImport => "'import'",
        TokenKind::KwIn => "'in'",
        TokenKind::KwInstance => "'instance'",
        TokenKind::KwLaw => "'law'",
        TokenKind::KwLet => "'let'",
        TokenKind::KwMatch => "'match'",
        TokenKind::KwMut => "'mut'",
        TokenKind::KwNeed => "'need'",
        TokenKind::KwNot => "'not'",
        TokenKind::KwOf => "'of'",
        TokenKind::KwOpaque => "'opaque'",
        TokenKind::KwOr => "'or'",
        TokenKind::KwQuote => "'quote'",
        TokenKind::KwRecord => "'record'",
        TokenKind::KwResume => "'resume'",
        TokenKind::KwReturn => "'return'",
        TokenKind::KwVia => "'via'",
        TokenKind::KwWhere => "'where'",
        TokenKind::KwWith => "'with'",
        TokenKind::KwXor => "'xor'",
        TokenKind::LParen => "'('",
        TokenKind::RParen => "')'",
        TokenKind::LBracket => "'['",
        TokenKind::RBracket => "']'",
        TokenKind::LBrace => "'{'",
        TokenKind::RBrace => "'}'",
        TokenKind::Semi => "';'",
        TokenKind::Comma => "','",
        TokenKind::Dot => "'.'",
        TokenKind::Colon => "':'",
        TokenKind::Pipe => "'|'",
        TokenKind::Bang => "'!'",
        TokenKind::Question => "'?'",
        TokenKind::Lt => "'<'",
        TokenKind::Gt => "'>'",
        TokenKind::Eq => "'='",
        TokenKind::ColonEq => "':='",
        TokenKind::LtMinus => "'<-'",
        TokenKind::MinusGt => "'->'",
        TokenKind::TildeGt => "'~>'",
        TokenKind::EqGt => "'=>'",
        TokenKind::SlashEq => "'/='",
        TokenKind::LtEq => "'<='",
        TokenKind::GtEq => "'>='",
        TokenKind::DotDot => "'..'",
        TokenKind::DotDotLt => "'..<'",
        TokenKind::DotDotDot => "'...'",
        TokenKind::DotLBracket => "'.['",
        TokenKind::DotLBrace => "'.{'",
        TokenKind::ColonQuestion => "':?'",
        TokenKind::ColonQuestionGt => "':?>'",
        TokenKind::LtColon => "'<:'",
        TokenKind::PipeGt => "'|>'",
        TokenKind::ColonColon => "'::'",
        TokenKind::QuestionDot => "'?.'",
        TokenKind::BangDot => "'!.'",
        TokenKind::QuestionQuestion => "'??'",
        TokenKind::At => "'@'",
        TokenKind::Hash => "'#'",
        TokenKind::HashLParen => "'#('",
        TokenKind::HashLBracket => "'#['",
        TokenKind::Plus => "'+'",
        TokenKind::Minus => "'-'",
        TokenKind::Star => "'*'",
        TokenKind::Slash => "'/'",
        TokenKind::Percent => "'%'",
        TokenKind::Eof => "end of input",
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
