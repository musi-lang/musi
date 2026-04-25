use music_hir::{HirTyId, HirTyKind};

use crate::api::{SurfaceTyId, SurfaceTyKind};

pub(super) struct RangeTyForm<Id> {
    pub(super) bound: Id,
}

pub(super) const fn range_to_surface_kind(bound: SurfaceTyId) -> SurfaceTyKind {
    SurfaceTyKind::Range { bound }
}

pub(super) const fn range_to_hir_kind(bound: HirTyId) -> HirTyKind {
    HirTyKind::Range { bound }
}

pub(super) const fn hir_range_form(kind: &HirTyKind) -> Option<RangeTyForm<HirTyId>> {
    match kind {
        HirTyKind::Range { bound } => Some(RangeTyForm { bound: *bound }),
        _ => None,
    }
}

pub(super) const fn surface_range_form(kind: &SurfaceTyKind) -> Option<RangeTyForm<SurfaceTyId>> {
    if let SurfaceTyKind::Range { bound } = kind {
        return Some(RangeTyForm { bound: *bound });
    }
    None
}
pub(super) fn simple_surface_ty_kind(kind: &HirTyKind) -> Option<SurfaceTyKind> {
    SimpleTyKind::from_hir(kind).map(SimpleTyKind::into_surface)
}

pub(super) fn simple_hir_ty_kind(kind: &SurfaceTyKind) -> Option<HirTyKind> {
    SimpleTyKind::from_surface(kind).map(SimpleTyKind::into_hir)
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum SimpleTyKind {
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
    NatLit(u64),
}

pub(super) struct SimpleTyInfo {
    simple: SimpleTyKind,
    hir: HirTyKind,
    surface: SurfaceTyKind,
    pub(super) display_name: &'static str,
}

const SIMPLE_TY_INFOS: &[SimpleTyInfo] = &[
    SimpleTyInfo {
        simple: SimpleTyKind::Error,
        hir: HirTyKind::Error,
        surface: SurfaceTyKind::Error,
        display_name: "<error>",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Unknown,
        hir: HirTyKind::Unknown,
        surface: SurfaceTyKind::Unknown,
        display_name: "Unknown",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Type,
        hir: HirTyKind::Type,
        surface: SurfaceTyKind::Type,
        display_name: "Type",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Syntax,
        hir: HirTyKind::Syntax,
        surface: SurfaceTyKind::Syntax,
        display_name: "Syntax",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Any,
        hir: HirTyKind::Any,
        surface: SurfaceTyKind::Any,
        display_name: "Any",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Empty,
        hir: HirTyKind::Empty,
        surface: SurfaceTyKind::Empty,
        display_name: "Empty",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Unit,
        hir: HirTyKind::Unit,
        surface: SurfaceTyKind::Unit,
        display_name: "Unit",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Bool,
        hir: HirTyKind::Bool,
        surface: SurfaceTyKind::Bool,
        display_name: "Bool",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Nat,
        hir: HirTyKind::Nat,
        surface: SurfaceTyKind::Nat,
        display_name: "Nat",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Int,
        hir: HirTyKind::Int,
        surface: SurfaceTyKind::Int,
        display_name: "Int",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Int8,
        hir: HirTyKind::Int8,
        surface: SurfaceTyKind::Int8,
        display_name: "Int8",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Int16,
        hir: HirTyKind::Int16,
        surface: SurfaceTyKind::Int16,
        display_name: "Int16",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Int32,
        hir: HirTyKind::Int32,
        surface: SurfaceTyKind::Int32,
        display_name: "Int32",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Int64,
        hir: HirTyKind::Int64,
        surface: SurfaceTyKind::Int64,
        display_name: "Int64",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Nat8,
        hir: HirTyKind::Nat8,
        surface: SurfaceTyKind::Nat8,
        display_name: "Nat8",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Nat16,
        hir: HirTyKind::Nat16,
        surface: SurfaceTyKind::Nat16,
        display_name: "Nat16",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Nat32,
        hir: HirTyKind::Nat32,
        surface: SurfaceTyKind::Nat32,
        display_name: "Nat32",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Nat64,
        hir: HirTyKind::Nat64,
        surface: SurfaceTyKind::Nat64,
        display_name: "Nat64",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Float,
        hir: HirTyKind::Float,
        surface: SurfaceTyKind::Float,
        display_name: "Float",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Float32,
        hir: HirTyKind::Float32,
        surface: SurfaceTyKind::Float32,
        display_name: "Float32",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Float64,
        hir: HirTyKind::Float64,
        surface: SurfaceTyKind::Float64,
        display_name: "Float64",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::String,
        hir: HirTyKind::String,
        surface: SurfaceTyKind::String,
        display_name: "String",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::Rune,
        hir: HirTyKind::Rune,
        surface: SurfaceTyKind::Rune,
        display_name: "Rune",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::CString,
        hir: HirTyKind::CString,
        surface: SurfaceTyKind::CString,
        display_name: "CString",
    },
    SimpleTyInfo {
        simple: SimpleTyKind::CPtr,
        hir: HirTyKind::CPtr,
        surface: SurfaceTyKind::CPtr,
        display_name: "CPtr",
    },
];

pub(super) fn simple_ty_info(kind: SimpleTyKind) -> Option<&'static SimpleTyInfo> {
    SIMPLE_TY_INFOS
        .iter()
        .find(|candidate| candidate.simple == kind)
}

impl SimpleTyKind {
    pub(super) fn from_hir(kind: &HirTyKind) -> Option<Self> {
        if let HirTyKind::NatLit(value) = kind {
            return Some(Self::NatLit(*value));
        }
        SIMPLE_TY_INFOS
            .iter()
            .find_map(|ty| (&ty.hir == kind).then_some(ty.simple))
    }

    pub(super) fn from_surface(kind: &SurfaceTyKind) -> Option<Self> {
        if let SurfaceTyKind::NatLit(value) = kind {
            return Some(Self::NatLit(*value));
        }
        SIMPLE_TY_INFOS
            .iter()
            .find_map(|ty| (&ty.surface == kind).then_some(ty.simple))
    }

    fn into_hir(self) -> HirTyKind {
        if let Self::NatLit(value) = self {
            return HirTyKind::NatLit(value);
        }
        simple_ty_info(self)
            .map(|ty| ty.hir.clone())
            .expect("simple type kind missing HIR mapping")
    }

    fn into_surface(self) -> SurfaceTyKind {
        if let Self::NatLit(value) = self {
            return SurfaceTyKind::NatLit(value);
        }
        simple_ty_info(self)
            .map(|ty| ty.surface.clone())
            .expect("simple type kind missing surface mapping")
    }
}
