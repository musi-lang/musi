use music_arena::SliceRange;
use music_names::Ident;

use crate::module::HirTyId;
use crate::origin::HirOrigin;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirTy {
    pub origin: HirOrigin,
    pub kind: HirTyKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirTyKind {
    Error,
    Named {
        name: Ident,
    },
    Apply {
        callee: HirTyId,
        args: SliceRange<HirTyId>,
    },
    Arrow {
        from: HirTyId,
        to: HirTyId,
        is_effectful: bool,
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirDim {
    Unknown,
    Name(Ident),
    Int(u32),
}
