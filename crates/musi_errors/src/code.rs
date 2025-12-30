use std::fmt;

/// Unique identifier for compiler errors.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum ErrorCode {
    // Lexer Errors (1xxx)
    /// Unknown character in source.
    E1001,
    /// String literal was not closed.
    E1002,
    /// Template literal was not closed.
    E1003,
    /// Escaped identifier was not closed.
    E1004,
    /// Rune literal was not closed.
    E1005,
    /// Block comment was not closed.
    E1006,
    /// Identifier format is invalid.
    E1007,
    /// Rune literal is invalid.
    E1008,
    /// Numeric literal is malformed.
    E1009,
    /// Underscore placement is invalid.
    E1010,
    /// Escape sequence is unknown.
    E1011,

    // Parser Errors (2xxx)
    /// Generic parser error.
    E2001,
}

impl ErrorCode {
    /// Returns numeric representation of error code.
    #[must_use]
    pub const fn as_u16(self) -> u16 {
        match self {
            Self::E1001 => 1001,
            Self::E1002 => 1002,
            Self::E1003 => 1003,
            Self::E1004 => 1004,
            Self::E1005 => 1005,
            Self::E1006 => 1006,
            Self::E1007 => 1007,
            Self::E1008 => 1008,
            Self::E1009 => 1009,
            Self::E1010 => 1010,
            Self::E1011 => 1011,

            Self::E2001 => 2001,
        }
    }
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "MS{}", self.as_u16())
    }
}
