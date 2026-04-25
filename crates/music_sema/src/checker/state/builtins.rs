use music_hir::{HirOrigin, HirTy, HirTyId, HirTyKind};
use music_names::KnownSymbols;
use music_resolve::ResolvedModule;

#[derive(Debug, Clone, Copy)]
pub struct Builtins {
    pub error: HirTyId,
    pub unknown: HirTyId,
    pub type_: HirTyId,
    pub syntax: HirTyId,
    pub any: HirTyId,
    pub empty: HirTyId,
    pub unit: HirTyId,
    pub bool_: HirTyId,
    pub nat: HirTyId,
    pub int_: HirTyId,
    pub int8: HirTyId,
    pub int16: HirTyId,
    pub int32: HirTyId,
    pub int64: HirTyId,
    pub nat8: HirTyId,
    pub nat16: HirTyId,
    pub nat32: HirTyId,
    pub nat64: HirTyId,
    pub float_: HirTyId,
    pub float32: HirTyId,
    pub float64: HirTyId,
    pub string_: HirTyId,
    pub rune: HirTyId,
    pub cstring: HirTyId,
    pub cptr: HirTyId,
}

impl Builtins {
    pub fn from_resolved(resolved: &mut ResolvedModule, _known: KnownSymbols) -> Self {
        Self {
            error: alloc_builtin(resolved, HirTyKind::Error),
            unknown: alloc_builtin(resolved, HirTyKind::Unknown),
            type_: alloc_builtin(resolved, HirTyKind::Type),
            syntax: alloc_builtin(resolved, HirTyKind::Syntax),
            any: alloc_builtin(resolved, HirTyKind::Any),
            empty: alloc_builtin(resolved, HirTyKind::Empty),
            unit: alloc_builtin(resolved, HirTyKind::Unit),
            bool_: alloc_builtin(resolved, HirTyKind::Bool),
            nat: alloc_builtin(resolved, HirTyKind::Nat),
            int_: alloc_builtin(resolved, HirTyKind::Int),
            int8: alloc_builtin(resolved, HirTyKind::Int8),
            int16: alloc_builtin(resolved, HirTyKind::Int16),
            int32: alloc_builtin(resolved, HirTyKind::Int32),
            int64: alloc_builtin(resolved, HirTyKind::Int64),
            nat8: alloc_builtin(resolved, HirTyKind::Nat8),
            nat16: alloc_builtin(resolved, HirTyKind::Nat16),
            nat32: alloc_builtin(resolved, HirTyKind::Nat32),
            nat64: alloc_builtin(resolved, HirTyKind::Nat64),
            float_: alloc_builtin(resolved, HirTyKind::Float),
            float32: alloc_builtin(resolved, HirTyKind::Float32),
            float64: alloc_builtin(resolved, HirTyKind::Float64),
            string_: alloc_builtin(resolved, HirTyKind::String),
            rune: alloc_builtin(resolved, HirTyKind::Rune),
            cstring: alloc_builtin(resolved, HirTyKind::CString),
            cptr: alloc_builtin(resolved, HirTyKind::CPtr),
        }
    }
}

fn alloc_builtin(resolved: &mut ResolvedModule, kind: HirTyKind) -> HirTyId {
    resolved
        .module
        .store
        .alloc_ty(HirTy::new(HirOrigin::dummy(), kind))
}
