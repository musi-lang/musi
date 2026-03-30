use music_names::Symbol;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IrScalarTy {
    Unit,
    Bool,
    Int,
    Float,
    String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IrTypeRef {
    Scalar(IrScalarTy),
    Named(Symbol),
    Any,
    Unknown,
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrExprTy {
    Scalar(IrScalarTy),
    Named(Symbol),
    Tuple { arity: u16 },
    Array { elem: IrTypeRef },
    Record { fields: Box<[Symbol]> },
    Any,
    Unknown,
    Error,
}

impl IrExprTy {
    #[must_use]
    pub fn as_type_ref(&self) -> IrTypeRef {
        match self {
            Self::Scalar(s) => IrTypeRef::Scalar(*s),
            Self::Named(sym) => IrTypeRef::Named(*sym),
            Self::Any => IrTypeRef::Any,
            Self::Unknown => IrTypeRef::Unknown,
            Self::Error => IrTypeRef::Error,
            Self::Tuple { .. } | Self::Array { .. } | Self::Record { .. } => IrTypeRef::Unknown,
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
