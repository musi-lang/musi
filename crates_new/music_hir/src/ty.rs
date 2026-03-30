use music_basic::Span;
use music_names::Ident;
use music_storage::Idx;

use super::{HirOrigin, HirTyIds};

pub type HirTyId = Idx<HirTy>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirTy {
    pub origin: HirOrigin,
    pub kind: HirTyKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirArrowFlavor {
    Pure,
    Effectful,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HirTyBinOp {
    Sum,
    Product,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirDim {
    IntLit { span: Span, value: Option<u64> },
    Name { name: Ident },
    Inferred { span: Span },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirTyKind {
    Named {
        name: Ident,
        args: HirTyIds,
    },
    Arrow {
        flavor: HirArrowFlavor,
        input: HirTyId,
        output: HirTyId,
    },
    Binary {
        op: HirTyBinOp,
        left: HirTyId,
        right: HirTyId,
    },
    Mut {
        base: HirTyId,
    },
    Pi {
        binder: Ident,
        binder_ty: HirTyId,
        body: HirTyId,
    },
    Tuple {
        items: HirTyIds,
    },
    Array {
        dims: Box<[HirDim]>,
        elem: HirTyId,
    },
    Error,
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
