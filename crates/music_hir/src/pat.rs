use music_arena::SliceRange;
use music_names::Ident;

use crate::module::{HirExprId, HirPatId};
use crate::origin::HirOrigin;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirPat {
    pub origin: HirOrigin,
    pub kind: HirPatKind,
}

impl HirPat {
    #[must_use]
    pub const fn new(origin: HirOrigin, kind: HirPatKind) -> Self {
        Self { origin, kind }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirPatKind {
    Error,
    Wildcard,
    Bind {
        name: Ident,
    },
    Lit {
        expr: HirExprId,
    },
    Tuple {
        items: SliceRange<HirPatId>,
    },
    Array {
        items: SliceRange<HirPatId>,
    },
    Record {
        fields: SliceRange<HirRecordPatField>,
    },
    Variant {
        tag: Ident,
        args: SliceRange<HirPatId>,
    },
    Or {
        left: HirPatId,
        right: HirPatId,
    },
    As {
        pat: HirPatId,
        name: Ident,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirRecordPatField {
    pub name: Ident,
    pub value: Option<HirPatId>,
}

impl HirRecordPatField {
    #[must_use]
    pub const fn new(name: Ident, value: Option<HirPatId>) -> Self {
        Self { name, value }
    }
}
