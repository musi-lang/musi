use music_arena::SliceRange;
use music_names::{Ident, Symbol};

use crate::module::HirTyId;
use crate::origin::HirOrigin;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirTy {
    pub origin: HirOrigin,
    pub kind: HirTyKind,
}

impl HirTy {
    #[must_use]
    pub const fn new(origin: HirOrigin, kind: HirTyKind) -> Self {
        Self { origin, kind }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirTyKind {
    Error,
    Unknown,
    Type,
    Syntax,
    Any,
    Empty,
    Unit,
    Bool,
    Nat,
    Int,
    Float,
    String,
    CString,
    CPtr,
    Module,
    NatLit(u64),
    Named {
        name: Symbol,
        args: SliceRange<HirTyId>,
    },
    Pi {
        binder: Symbol,
        binder_ty: HirTyId,
        body: HirTyId,
        is_effectful: bool,
    },
    Arrow {
        params: SliceRange<HirTyId>,
        ret: HirTyId,
        is_effectful: bool,
    },
    Sum {
        left: HirTyId,
        right: HirTyId,
    },
    Tuple {
        items: SliceRange<HirTyId>,
    },
    Array {
        dims: SliceRange<HirDim>,
        item: HirTyId,
    },
    Mut {
        inner: HirTyId,
    },
    Record {
        fields: SliceRange<HirTyField>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirDim {
    Unknown,
    Name(Ident),
    Int(u32),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirTyField {
    pub name: Symbol,
    pub ty: HirTyId,
}

impl HirTyField {
    #[must_use]
    pub const fn new(name: Symbol, ty: HirTyId) -> Self {
        Self { name, ty }
    }
}
