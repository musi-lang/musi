use std::fmt;

/// Primitive types known to the compiler before any user code is parsed.
///
/// Each variant corresponds to a type declared in `std/prelude/mod.ms`.
/// The compiler erases these to NaN-box tags or heap pointers at codegen.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinType {
    // Compiler intrinsics
    Type,
    Any,
    Unknown,
    Never,
    // Erasable to NaN-box tags
    Unit,
    Bool,
    Int,
    Nat,
    Float,
    Rune,
    // Collections
    String,
    Array,
    List,
    // Sized integers
    Int8,
    Int16,
    Int32,
    Int64,
    // Sized unsigned integers
    Nat8,
    Nat16,
    Nat32,
    Nat64,
    // Sized floats
    Float32,
    Float64,
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
        Self::Nat,
        Self::Float,
        Self::Rune,
        Self::String,
        Self::Array,
        Self::List,
        Self::Int8,
        Self::Int16,
        Self::Int32,
        Self::Int64,
        Self::Nat8,
        Self::Nat16,
        Self::Nat32,
        Self::Nat64,
        Self::Float32,
        Self::Float64,
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
            Self::Nat => "Nat",
            Self::Float => "Float",
            Self::Rune => "Rune",
            Self::String => "String",
            Self::Array => "Array",
            Self::List => "List",
            Self::Int8 => "Int8",
            Self::Int16 => "Int16",
            Self::Int32 => "Int32",
            Self::Int64 => "Int64",
            Self::Nat8 => "Nat8",
            Self::Nat16 => "Nat16",
            Self::Nat32 => "Nat32",
            Self::Nat64 => "Nat64",
            Self::Float32 => "Float32",
            Self::Float64 => "Float64",
        }
    }

    /// Stable bytecode type identifier.
    ///
    /// Compiler intrinsics occupy 0xFFF0..0xFFFF. Remaining types get
    /// sequential IDs starting from 0xFFE0.
    #[must_use]
    pub const fn type_id(self) -> u16 {
        match self {
            Self::Type => 0xFFF0,
            Self::Any => 0xFFF1,
            Self::Unknown => 0xFFF2,
            Self::Never => 0xFFF3,
            Self::Unit => 0xFFF4,
            Self::Bool => 0xFFF5,
            Self::Int => 0xFFF6,
            Self::Float => 0xFFF7,
            Self::String => 0xFFF8,
            Self::Nat => 0xFFE0,
            Self::Rune => 0xFFE1,
            Self::Array => 0xFFE2,
            Self::List => 0xFFE3,
            Self::Int8 => 0xFFE4,
            Self::Int16 => 0xFFE5,
            Self::Int32 => 0xFFE6,
            Self::Int64 => 0xFFE7,
            Self::Nat8 => 0xFFE8,
            Self::Nat16 => 0xFFE9,
            Self::Nat32 => 0xFFEA,
            Self::Nat64 => 0xFFEB,
            Self::Float32 => 0xFFEC,
            Self::Float64 => 0xFFED,
        }
    }

    /// NaN-box tag for this type, or `None` for IEEE double (untagged).
    ///
    /// - `0b000` = PTR (heap-allocated object)
    /// - `0b001` = SMI (small integer)
    /// - `0b010` = Bool
    /// - `0b011` = Unit
    /// - `0b101` = Rune
    #[must_use]
    pub const fn nan_box_tag(self) -> Option<u8> {
        match self {
            Self::Int
            | Self::Nat
            | Self::Int8
            | Self::Int16
            | Self::Int32
            | Self::Int64
            | Self::Nat8
            | Self::Nat16
            | Self::Nat32
            | Self::Nat64 => Some(0b001),
            Self::Bool => Some(0b010),
            Self::Unit => Some(0b011),
            Self::Rune => Some(0b101),
            Self::Float | Self::Float32 | Self::Float64 => None,
            // Heap-allocated: String, Array, List, Never, Type, Any, Unknown
            Self::String
            | Self::Array
            | Self::List
            | Self::Never
            | Self::Type
            | Self::Any
            | Self::Unknown => Some(0b000),
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
