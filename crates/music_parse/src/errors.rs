use core::error::Error;
use core::fmt;

use music_lex::TokenKind;
use music_shared::diag::{Diag, DiagCode};
use music_shared::SourceId;
use music_shared::Span;

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
    TypeApplicationUsesBrackets,
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

    #[must_use]
    pub fn diagnostic(&self, source_id: SourceId) -> Diag {
        let (code, message, hint): (DiagCode, String, Option<String>) = match &self.kind {
            ParseErrorKind::ExpectedToken { expected, found } => (
                DiagCode::new(2001),
                format!("expected {expected}, found {found}"),
                None,
            ),
            ParseErrorKind::TypeApplicationUsesBrackets => (
                DiagCode::new(2009),
                String::from("type application uses brackets"),
                Some(String::from("write bracketed type arguments, for example 'Option[Int]'")),
            ),
            ParseErrorKind::UnexpectedEof { expected } => (
                DiagCode::new(2002),
                format!("expected {expected}, found end of file"),
                None,
            ),
            ParseErrorKind::ExpectedExpr { found } => (
                DiagCode::new(2003),
                format!("expected expression, found {found}"),
                None,
            ),
            ParseErrorKind::ExpectedPat { found } => (
                DiagCode::new(2004),
                format!("expected pattern, found {found}"),
                None,
            ),
            ParseErrorKind::ExpectedType { found } => (
                DiagCode::new(2005),
                format!("expected type, found {found}"),
                None,
            ),
            ParseErrorKind::UnclosedDelimiter { open, .. } => (
                DiagCode::new(2006),
                format!("unclosed delimiter {open}"),
                None,
            ),
            ParseErrorKind::InvalidParenForm => (
                DiagCode::new(2007),
                String::from("invalid parenthesized form"),
                None,
            ),
            ParseErrorKind::NonAssociativeChain => (
                DiagCode::new(2008),
                String::from("non-associative operator chain is not allowed"),
                None,
            ),
        };

        let mut diag = Diag::error(message)
            .with_code(code)
            .with_label(self.span, source_id, "");
        if let Some(hint) = hint {
            diag = diag.with_hint(hint);
        }
        if let Some(ctx) = self.context {
            diag = diag.with_note(format!("while parsing {ctx}"));
        }
        diag
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
            Self::TypeApplicationUsesBrackets => {
                write!(f, "type application uses brackets")
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
        TokenKind::KwCase => "'case'",
        TokenKind::KwClass => "'class'",
        TokenKind::KwData => "'data'",
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
        TokenKind::KwMut => "'mut'",
        TokenKind::KwPerform => "'perform'",
        TokenKind::KwNot => "'not'",
        TokenKind::KwOf => "'of'",
        TokenKind::KwOpaque => "'opaque'",
        TokenKind::KwOr => "'or'",
        TokenKind::KwQuote => "'quote'",
        TokenKind::KwResume => "'resume'",
        TokenKind::KwReturn => "'return'",
        TokenKind::KwShl => "'shl'",
        TokenKind::KwShr => "'shr'",
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
