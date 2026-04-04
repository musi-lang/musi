use music_arena::SliceRange;
use music_names::{Ident, Symbol};

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
    Unknown,
    Type,
    Syntax,
    Any,
    Empty,
    Unit,
    Bool,
    Int,
    Float,
    String,
    CString,
    CPtr,
    Module,
    Named {
        name: Symbol,
        args: SliceRange<HirTyId>,
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
