use std::fmt;

/// Primitive types known to the compiler before any user code is parsed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinType {
    Type,
    Any,
    Unknown,
    Never,
    Unit,
    Bool,
    Int,
    Float,
    String,
    Rune,
}

impl BuiltinType {
    /// All built-in types in declaration order.
    pub const ALL: &[Self] = &[
        Self::Type,
        Self::Any,
        Self::Unknown,
        Self::Never,
        Self::Unit,
        Self::Bool,
        Self::Int,
        Self::Float,
        Self::String,
        Self::Rune,
    ];

    /// The source-level name of this built-in type.
    #[must_use]
    pub const fn name(self) -> &'static str {
        match self {
            Self::Type => "Type",
            Self::Any => "Any",
            Self::Unknown => "Unknown",
            Self::Never => "Never",
            Self::Unit => "Unit",
            Self::Bool => "Bool",
            Self::Int => "Int",
            Self::Float => "Float",
            Self::String => "String",
            Self::Rune => "Rune",
        }
    }
}

impl fmt::Display for BuiltinType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
