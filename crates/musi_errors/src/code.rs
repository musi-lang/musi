use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum ErrorCode {
    // Lexer Errors (1xxx)
    E1001, // unknown char
    E1002, // unclosed string
    E1003, // unclosed template
    E1004, // unclosed escaped ident
    E1005, // unclosed rune
    E1006, // unclosed block comment
    E1007, // invalid ident
    E1008, // invalid rune
    E1009, // malformed numeric
    E1010, // malformed underscore
    E1011, // unknown escape

    // Parser Errors (2xxx)
    E2001,
}

impl ErrorCode {
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
