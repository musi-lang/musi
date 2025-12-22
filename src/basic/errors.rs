use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Level {
    Error,
    Warning,
    Note,
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum NumericError {
    #[error("malformed underscore in {0} literal")]
    InvalidUnderscore(String),
    #[error("invalid {0} literal")]
    NoDigits(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum LexicalError {
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
    #[error("unknown character '{0}'")]
    UnknownChar(char),
    #[error("malformed underscore in {0} literal")]
    MalformedUnderscore(String),
    #[error("rune literal cannot be empty")]
    EmptyRune,
    #[error("rune literal must contain exactly one character")]
    MultiCharRune,
    #[error(transparent)]
    InvalidNumeric(#[from] NumericError),
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum Error {
    #[error(transparent)]
    Lexical(#[from] LexicalError),
}

impl NumericError {
    pub fn hint(&self) -> Option<&'static str> {
        match self {
            Self::InvalidUnderscore(_) => Some("underscores must separate digits"),
            Self::NoDigits(_) => None,
        }
    }

    pub const fn level(&self) -> Level {
        Level::Error
    }
}

impl LexicalError {
    pub fn hint(&self) -> Option<&'static str> {
        match self {
            Self::UnclosedString | Self::UnclosedTemplate => Some("missing '\"'"),
            Self::UnclosedEscapedIdent => Some("missing '`'"),
            Self::UnclosedRune => Some("missing '\''"),
            Self::UnclosedComment => Some("missing '*/'"),
            Self::UnknownChar(_) => Some("remove this character"),
            Self::MalformedUnderscore(_) => Some("underscores must separate digits"),
            Self::InvalidIdent | Self::EmptyRune | Self::MultiCharRune => None,
            Self::InvalidNumeric(e) => e.hint(),
        }
    }

    pub const fn level(&self) -> Level {
        match self {
            Self::InvalidNumeric(e) => e.level(),
            _ => Level::Error,
        }
    }
}

impl Error {
    pub fn hint(&self) -> Option<&'static str> {
        match self {
            Self::Lexical(e) => e.hint(),
        }
    }

    pub const fn level(&self) -> Level {
        match self {
            Self::Lexical(e) => e.level(),
        }
    }
}
