use music_names::{Symbol, SymbolSlice};

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
    Record { fields: SymbolSlice },
    Any,
    Unknown,
    Error,
}

impl IrExprTy {
    #[must_use]
    pub const fn as_ty_ref(&self) -> IrTypeRef {
        match self {
            Self::Scalar(s) => IrTypeRef::Scalar(*s),
            Self::Named(sym) => IrTypeRef::Named(*sym),
            Self::Any => IrTypeRef::Any,
            Self::Error => IrTypeRef::Error,
            Self::Unknown | Self::Tuple { .. } | Self::Array { .. } | Self::Record { .. } => {
                IrTypeRef::Unknown
            }
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
