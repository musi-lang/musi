use music_arena::SliceRange;
use music_names::Ident;

use crate::module::{HirExprId, HirPatId};
use crate::origin::HirOrigin;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirPat {
    pub origin: HirOrigin,
    pub kind: HirPatKind,
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
    pub is_mut: bool,
    pub name: Ident,
    pub value: Option<HirPatId>,
}
