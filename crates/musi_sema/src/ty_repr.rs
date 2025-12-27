use std::fmt;

use crate::symbol::SymbolId;
use crate::types::{TyReprPtr, TyReprs};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyVarId(pub u32);

impl TyVarId {
    #[must_use]
    pub const fn new(id: u32) -> Self {
        Self(id)
    }

    #[must_use]
    pub const fn as_u32(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntWidth {
    I8,
    I16,
    I32,
    I64,
}

impl IntWidth {
    #[must_use]
    pub const fn bits(self) -> u8 {
        match self {
            Self::I8 => 8,
            Self::I16 => 16,
            Self::I32 => 32,
            Self::I64 => 64,
        }
    }

    #[must_use]
    pub const fn native() -> Self {
        #[cfg(target_pointer_width = "64")]
        {
            Self::I64
        }
        #[cfg(target_pointer_width = "32")]
        {
            Self::I32
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FloatWidth {
    F32,
    F64,
}

impl FloatWidth {
    #[must_use]
    pub const fn bits(self) -> u8 {
        match self {
            Self::F32 => 32,
            Self::F64 => 64,
        }
    }

    #[must_use]
    pub const fn native() -> Self {
        #[cfg(target_pointer_width = "64")]
        {
            Self::F64
        }
        #[cfg(target_pointer_width = "32")]
        {
            Self::F32
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyRepr {
    pub kind: TyReprKind,
}

impl TyRepr {
    #[must_use]
    pub const fn new(kind: TyReprKind) -> Self {
        Self { kind }
    }

    #[must_use]
    pub const fn unit() -> Self {
        Self::new(TyReprKind::Unit)
    }

    #[must_use]
    pub const fn bool() -> Self {
        Self::new(TyReprKind::Bool)
    }

    #[must_use]
    pub const fn never() -> Self {
        Self::new(TyReprKind::Never)
    }

    #[must_use]
    pub const fn any() -> Self {
        Self::new(TyReprKind::Any)
    }

    #[must_use]
    pub const fn unknown() -> Self {
        Self::new(TyReprKind::Unknown)
    }

    #[must_use]
    pub const fn error() -> Self {
        Self::new(TyReprKind::Error)
    }

    #[must_use]
    pub const fn string() -> Self {
        Self::new(TyReprKind::String)
    }

    #[must_use]
    pub const fn rune() -> Self {
        Self::new(TyReprKind::Rune)
    }

    #[must_use]
    pub const fn int(width: IntWidth) -> Self {
        Self::new(TyReprKind::int(width))
    }

    #[must_use]
    pub const fn nat(width: IntWidth) -> Self {
        Self::new(TyReprKind::nat(width))
    }

    #[must_use]
    pub const fn float(width: FloatWidth) -> Self {
        Self::new(TyReprKind::float(width))
    }

    #[must_use]
    pub const fn var(id: TyVarId) -> Self {
        Self::new(TyReprKind::Var(id))
    }

    #[must_use]
    pub fn optional(inner: Self) -> Self {
        Self::new(TyReprKind::optional(inner))
    }

    #[must_use]
    pub fn ptr(inner: Self) -> Self {
        Self::new(TyReprKind::ptr(inner))
    }

    #[must_use]
    pub fn array(elem: Self, size: Option<usize>) -> Self {
        Self::new(TyReprKind::array(elem, size))
    }

    #[must_use]
    pub const fn tuple(elems: Vec<Self>) -> Self {
        Self::new(TyReprKind::Tuple(elems))
    }

    #[must_use]
    pub fn func(params: Vec<Self>, ret: Self) -> Self {
        Self::new(TyReprKind::func(params, ret))
    }

    #[must_use]
    pub const fn named(symbol: SymbolId, args: Vec<Self>) -> Self {
        Self::new(TyReprKind::named(symbol, args))
    }

    #[must_use]
    pub const fn is_error(&self) -> bool {
        matches!(self.kind, TyReprKind::Error)
    }

    #[must_use]
    pub const fn is_any(&self) -> bool {
        matches!(self.kind, TyReprKind::Any)
    }

    #[must_use]
    pub const fn is_var(&self) -> bool {
        matches!(self.kind, TyReprKind::Var(_))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyReprKind {
    Int(IntWidth),
    Nat(IntWidth),
    Float(FloatWidth),
    Bool,
    Rune,
    String,
    Unit,
    Never,
    Any,
    Unknown,
    Tuple(TyReprs),
    Array(TyReprPtr, Option<usize>),
    Ptr(TyReprPtr),
    Optional(TyReprPtr),
    Fn(TyReprs, TyReprPtr),
    Named(SymbolId, TyReprs),
    Var(TyVarId),
    Error,
}

impl TyReprKind {
    #[must_use]
    pub const fn int(width: IntWidth) -> Self {
        Self::Int(width)
    }

    #[must_use]
    pub const fn nat(width: IntWidth) -> Self {
        Self::Nat(width)
    }

    #[must_use]
    pub const fn float(width: FloatWidth) -> Self {
        Self::Float(width)
    }

    #[must_use]
    pub const fn tuple(elems: TyReprs) -> Self {
        Self::Tuple(elems)
    }

    #[must_use]
    pub fn array(elem: TyRepr, size: Option<usize>) -> Self {
        Self::Array(Box::new(elem), size)
    }

    #[must_use]
    pub fn ptr(inner: TyRepr) -> Self {
        Self::Ptr(Box::new(inner))
    }

    #[must_use]
    pub fn optional(inner: TyRepr) -> Self {
        Self::Optional(Box::new(inner))
    }

    #[must_use]
    pub fn func(params: TyReprs, ret: TyRepr) -> Self {
        Self::Fn(params, Box::new(ret))
    }

    #[must_use]
    pub const fn named(symbol: SymbolId, args: TyReprs) -> Self {
        Self::Named(symbol, args)
    }
}

impl fmt::Display for TyRepr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            TyReprKind::Int(w) => write!(f, "Int{}", w.bits()),
            TyReprKind::Nat(w) => write!(f, "Nat{}", w.bits()),
            TyReprKind::Float(w) => write!(f, "Float{}", w.bits()),
            TyReprKind::Bool => write!(f, "Bool"),
            TyReprKind::Rune => write!(f, "Rune"),
            TyReprKind::String => write!(f, "String"),
            TyReprKind::Unit => write!(f, "()"),
            TyReprKind::Never => write!(f, "Never"),
            TyReprKind::Any => write!(f, "Any"),
            TyReprKind::Unknown => write!(f, "Unknown"),
            TyReprKind::Tuple(elems) => {
                write!(f, "(")?;
                write_comma_separated(f, elems)?;
                write!(f, ")")
            }
            TyReprKind::Array(elem, size) => {
                if let Some(n) = size {
                    write!(f, "[{n}]{elem}")
                } else {
                    write!(f, "[]{elem}")
                }
            }
            TyReprKind::Ptr(inner) => write!(f, "^{inner}"),
            TyReprKind::Optional(inner) => write!(f, "?{inner}"),
            TyReprKind::Fn(params, ret) => {
                write!(f, "(")?;
                write_comma_separated(f, params)?;
                write!(f, ") -> {ret}")
            }
            TyReprKind::Named(symbol, args) => {
                write!(f, "Named#{}", symbol.0)?;
                if !args.is_empty() {
                    write!(f, "[")?;
                    write_comma_separated(f, args)?;
                    write!(f, "]")?;
                }
                Ok(())
            }
            TyReprKind::Var(id) => write!(f, "?T{}", id.0),
            TyReprKind::Error => write!(f, "<error>"),
        }
    }
}

fn write_comma_separated(f: &mut fmt::Formatter<'_>, elems: &[TyRepr]) -> fmt::Result {
    for (i, elem) in elems.iter().enumerate() {
        if i > 0 {
            write!(f, ", ")?;
        }
        write!(f, "{elem}")?;
    }
    Ok(())
}
