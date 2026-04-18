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
    Int8,
    Int16,
    Int32,
    Int64,
    Nat8,
    Nat16,
    Nat32,
    Nat64,
    Float,
    Float32,
    Float64,
    String,
    Rune,
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
    Seq {
        item: HirTyId,
    },
    Array {
        dims: SliceRange<HirDim>,
        item: HirTyId,
    },
    Range {
        bound: HirTyId,
    },
    ClosedRange {
        bound: HirTyId,
    },
    PartialRangeFrom {
        bound: HirTyId,
    },
    PartialRangeUpTo {
        bound: HirTyId,
    },
    PartialRangeThru {
        bound: HirTyId,
    },
    Handler {
        effect: HirTyId,
        input: HirTyId,
        output: HirTyId,
    },
    Mut {
        inner: HirTyId,
    },
    AnyClass {
        class: HirTyId,
    },
    SomeClass {
        class: HirTyId,
    },
    Record {
        fields: SliceRange<HirTyField>,
    },
}

pub struct SimpleHirTyInfo {
    pub kind: HirTyKind,
    pub parse_name: &'static str,
    pub display_name: &'static str,
}

impl SimpleHirTyInfo {
    const fn new(kind: HirTyKind, parse_name: &'static str, display_name: &'static str) -> Self {
        Self {
            kind,
            parse_name,
            display_name,
        }
    }
}

pub const SIMPLE_HIR_TYS: &[SimpleHirTyInfo] = &[
    SimpleHirTyInfo::new(HirTyKind::Error, "Error", "<error>"),
    SimpleHirTyInfo::new(HirTyKind::Unknown, "Unknown", "Unknown"),
    SimpleHirTyInfo::new(HirTyKind::Type, "Type", "Type"),
    SimpleHirTyInfo::new(HirTyKind::Syntax, "Syntax", "Syntax"),
    SimpleHirTyInfo::new(HirTyKind::Any, "Any", "Any"),
    SimpleHirTyInfo::new(HirTyKind::Empty, "Empty", "Empty"),
    SimpleHirTyInfo::new(HirTyKind::Unit, "Unit", "Unit"),
    SimpleHirTyInfo::new(HirTyKind::Bool, "Bool", "Bool"),
    SimpleHirTyInfo::new(HirTyKind::Nat, "Nat", "Nat"),
    SimpleHirTyInfo::new(HirTyKind::Int, "Int", "Int"),
    SimpleHirTyInfo::new(HirTyKind::Int8, "Int8", "Int8"),
    SimpleHirTyInfo::new(HirTyKind::Int16, "Int16", "Int16"),
    SimpleHirTyInfo::new(HirTyKind::Int32, "Int32", "Int32"),
    SimpleHirTyInfo::new(HirTyKind::Int64, "Int64", "Int64"),
    SimpleHirTyInfo::new(HirTyKind::Nat8, "Nat8", "Nat8"),
    SimpleHirTyInfo::new(HirTyKind::Nat16, "Nat16", "Nat16"),
    SimpleHirTyInfo::new(HirTyKind::Nat32, "Nat32", "Nat32"),
    SimpleHirTyInfo::new(HirTyKind::Nat64, "Nat64", "Nat64"),
    SimpleHirTyInfo::new(HirTyKind::Float, "Float", "Float"),
    SimpleHirTyInfo::new(HirTyKind::Float32, "Float32", "Float32"),
    SimpleHirTyInfo::new(HirTyKind::Float64, "Float64", "Float64"),
    SimpleHirTyInfo::new(HirTyKind::String, "String", "String"),
    SimpleHirTyInfo::new(HirTyKind::Rune, "Rune", "Rune"),
    SimpleHirTyInfo::new(HirTyKind::CString, "CString", "CString"),
    SimpleHirTyInfo::new(HirTyKind::CPtr, "CPtr", "CPtr"),
    SimpleHirTyInfo::new(HirTyKind::Module, "Module", "Module"),
];

#[must_use]
pub fn simple_hir_ty_name(kind: &HirTyKind) -> Option<&'static str> {
    SIMPLE_HIR_TYS
        .iter()
        .find_map(|info| (&info.kind == kind).then_some(info.parse_name))
}

#[must_use]
pub fn simple_hir_ty_display_name(kind: &HirTyKind) -> Option<&'static str> {
    SIMPLE_HIR_TYS
        .iter()
        .find_map(|info| (&info.kind == kind).then_some(info.display_name))
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
