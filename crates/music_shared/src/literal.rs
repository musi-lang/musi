use std::fmt;

/// A literal value from source code.
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    /// Integer literal, e.g. `42`.
    Int(i64),
    /// Floating-point literal, e.g. `3.14`.
    Float(f64),
    /// String literal, e.g. `"hello"`.
    Str(String),
    /// Rune (character) literal, e.g. `'a'`.
    Rune(char),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(n) => write!(f, "{n}"),
            Self::Float(n) => write!(f, "{n}"),
            Self::Str(s) => write!(f, "\"{s}\""),
            Self::Rune(c) => write!(f, "'{c}'"),
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
