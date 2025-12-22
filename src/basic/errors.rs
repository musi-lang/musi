use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Level {
    Error,
    Warning,
    Note,
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum LexErrorKind {
    #[error("unclosed string literal")]
    UnclosedString,
    #[error("unclosed template literal")]
    UnclosedTemplate,
    #[error("unclosed escaped identifier")]
    UnclosedEscapedIdent,
    #[error("unclosed rune literal")]
    UnclosedRune,
    #[error("unclosed block comment")]
    UnclosedComment,
    #[error("invalid identifier")]
    InvalidIdent,
    #[error("invalid character '{0}'")]
    InvalidChar(char),
    #[error("malformed underscore in {0} literal")]
    MalformedUnderscore(String),
    #[error("invalid {0} literal")]
    InvalidLiteral(String),
    #[error("rune literal cannot be empty")]
    EmptyRune,
    #[error("rune literal must contain exactly one character")]
    MultiCharRune,
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum ErrorKind {
    #[error(transparent)]
    Lexical(#[from] LexErrorKind),
}

impl LexErrorKind {
    pub fn hint(&self) -> Option<&'static str> {
        match self {
            Self::UnclosedString | Self::UnclosedTemplate => Some("missing '\"'"),
            Self::UnclosedEscapedIdent => Some("missing '`'"),
            Self::UnclosedRune => Some("missing '\''"),
            Self::UnclosedComment => Some("missing '*/'"),
            Self::InvalidChar(_) => Some("remove this character"),
            Self::MalformedUnderscore(_) => Some("underscores must separate digits"),
            Self::InvalidIdent
            | Self::InvalidLiteral(_)
            | Self::EmptyRune
            | Self::MultiCharRune => None,
        }
    }

    pub const fn level(&self) -> Level {
        Level::Error
    }
}

impl ErrorKind {
    pub fn hint(&self) -> Option<&'static str> {
        match self {
            Self::Lexical(err) => err.hint(),
        }
    }

    pub const fn level(&self) -> Level {
        match self {
            Self::Lexical(err) => err.level(),
        }
    }
}
