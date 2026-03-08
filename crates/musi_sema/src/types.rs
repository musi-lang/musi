//! Semantic type representation used by the Musi type checker.
//!
//! These types are produced by the type checker from the AST types ([`musi_parse::ast::Ty`])
//! and placed in the side table returned by [`crate::analyze`].

use core::fmt;

use crate::def::DefId;

/// A unique identifier for a unification variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVarId(pub u32);

/// Built-in primitive types in Musi.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimTy {
    // Platform-sized
    Int,
    Nat,
    Float,
    // Fixed-width signed
    Int8,
    Int16,
    Int32,
    Int64,
    // Fixed-width unsigned
    Nat8,
    Nat16,
    Nat32,
    Nat64,
    // Fixed-width float
    Float32,
    Float64,
    // Text
    Rune,
    String,
    // Other
    Bool,
    Unit,
    Any,
}

impl PrimTy {
    /// Returns the source-level name of this primitive type.
    #[must_use]
    pub const fn name(self) -> &'static str {
        match self {
            Self::Int => "Int",
            Self::Nat => "Nat",
            Self::Float => "Float",
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
            Self::Rune => "Rune",
            Self::String => "String",
            Self::Bool => "Bool",
            Self::Unit => "Unit",
            Self::Any => "Any",
        }
    }

    /// Returns `Some(PrimTy)` if the given name matches a primitive type.
    #[must_use]
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            "Int" => Some(Self::Int),
            "Nat" => Some(Self::Nat),
            "Float" => Some(Self::Float),
            "Int8" => Some(Self::Int8),
            "Int16" => Some(Self::Int16),
            "Int32" => Some(Self::Int32),
            "Int64" => Some(Self::Int64),
            "Nat8" => Some(Self::Nat8),
            "Nat16" => Some(Self::Nat16),
            "Nat32" => Some(Self::Nat32),
            "Nat64" => Some(Self::Nat64),
            "Float32" => Some(Self::Float32),
            "Float64" => Some(Self::Float64),
            "Rune" => Some(Self::Rune),
            "String" => Some(Self::String),
            "Bool" => Some(Self::Bool),
            "Unit" => Some(Self::Unit),
            "Any" => Some(Self::Any),
            _ => None,
        }
    }
}

impl fmt::Display for PrimTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

/// A resolved semantic type.
///
/// This is distinct from the syntactic [`musi_parse::ast::Ty`]; it is produced
/// after name resolution and during type inference.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// A built-in primitive type.
    Prim(PrimTy),
    /// A product type (tuple).
    Tuple(Vec<Self>),
    /// An array type with an optional fixed size.
    Array(Box<Self>, Option<usize>),
    /// A function type: parameters → return.
    Arrow(Vec<Self>, Box<Self>),
    /// A user-defined named type with type arguments.
    Named(DefId, Vec<Self>),
    /// An open unification variable.
    Var(TypeVarId),
    /// A poison type that suppresses cascading errors.
    Error,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Prim(p) => write!(f, "{p}"),
            Self::Tuple(elems) => {
                write!(f, "(")?;
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{e}")?;
                }
                write!(f, ")")
            }
            Self::Array(elem, size) => {
                // Musi syntax: []elem or [N]elem
                if let Some(n) = size {
                    write!(f, "[{n}]{elem}")
                } else {
                    write!(f, "[]{elem}")
                }
            }
            Self::Arrow(params, ret) => {
                if params.len() == 1 {
                    write!(f, "{} -> {ret}", &params[0])
                } else {
                    write!(f, "(")?;
                    for (i, p) in params.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{p}")?;
                    }
                    write!(f, ") -> {ret}")
                }
            }
            Self::Named(id, args) => {
                // Without interner access we fall back to the DefId; callers that have
                // interner context should use fmt_ty_err() instead.
                write!(f, "?type{}", id.0)?;
                if !args.is_empty() {
                    write!(f, "[")?;
                    for (i, a) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{a}")?;
                    }
                    write!(f, "]")?;
                }
                Ok(())
            }
            Self::Var(v) => write!(f, "?{}", v.0),
            Self::Error => write!(f, "<error>"),
        }
    }
}
