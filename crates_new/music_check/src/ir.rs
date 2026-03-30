use std::collections::HashMap;

use music_basic::SourceId;
use music_hir::{HirExprId, HirExprKind, HirPatKind, HirStore};
use music_ir::{IrDataLayout, IrDataLayouts, IrExprTy, IrModuleInfo, IrScalarTy, IrTypeRef};
use music_known::KnownSymbols;
use music_names::{Interner, NameBindingId, NameResolution, NameSite};

use crate::{SemTy, SemTyId, SemTys, unify};

pub fn build_ir_module(
    known: KnownSymbols,
    interner: &Interner,
    semtys: &SemTys,
    expr_tys: &HashMap<HirExprId, SemTyId>,
    store: &HirStore,
    names: &NameResolution,
    source_id: SourceId,
) -> IrModuleInfo {
    let mut out = vec![IrExprTy::Unknown; store.exprs.len()];
    for (&expr_id, &sem_ty) in expr_tys {
        let idx = usize::try_from(expr_id.raw()).unwrap_or(usize::MAX);
        if idx >= out.len() {
            continue;
        }
        out[idx] = lower_sem_ty_to_ir(known, interner, semtys, sem_ty);
    }

    let data_layouts = collect_data_layouts(store, names, source_id);

    IrModuleInfo {
        expr_tys: out.into_boxed_slice(),
        data_layouts,
    }
}

fn lower_sem_ty_to_ir(
    known: KnownSymbols,
    interner: &Interner,
    semtys: &SemTys,
    ty: SemTyId,
) -> IrExprTy {
    let ty = unify::resolve(semtys, ty);
    match semtys.get(ty).clone() {
        SemTy::Error => IrExprTy::Error,
        SemTy::Unknown
        | SemTy::InferVar(_)
        | SemTy::Generic(_)
        | SemTy::Arrow { .. }
        | SemTy::Binary { .. }
        | SemTy::Mut { .. } => IrExprTy::Unknown,
        SemTy::Any => IrExprTy::Any,
        SemTy::Named { name, .. } => match name {
            n if n == known.unit => IrExprTy::Scalar(IrScalarTy::Unit),
            n if n == known.bool_ => IrExprTy::Scalar(IrScalarTy::Bool),
            n if n == known.int_ => IrExprTy::Scalar(IrScalarTy::Int),
            n if n == known.float_ => IrExprTy::Scalar(IrScalarTy::Float),
            n if n == known.string_ => IrExprTy::Scalar(IrScalarTy::String),
            _ => IrExprTy::Named(name),
        },
        SemTy::Tuple { items } => IrExprTy::Tuple {
            arity: u16::try_from(items.len()).unwrap_or(u16::MAX),
        },
        SemTy::Array { elem, .. } => IrExprTy::Array {
            elem: lower_sem_ty_to_ref(known, interner, semtys, elem),
        },
        SemTy::Record { fields } => IrExprTy::Record {
            fields: {
                let mut syms = fields.keys().copied().collect::<Vec<_>>();
                syms.sort_by(|a, b| interner.resolve(*a).cmp(interner.resolve(*b)));
                syms.into_boxed_slice()
            },
        },
    }
}

fn lower_sem_ty_to_ref(
    known: KnownSymbols,
    interner: &Interner,
    semtys: &SemTys,
    ty: SemTyId,
) -> IrTypeRef {
    match lower_sem_ty_to_ir(known, interner, semtys, ty) {
        IrExprTy::Scalar(s) => IrTypeRef::Scalar(s),
        IrExprTy::Named(sym) => IrTypeRef::Named(sym),
        IrExprTy::Any => IrTypeRef::Any,
        IrExprTy::Error => IrTypeRef::Error,
        _ => IrTypeRef::Unknown,
    }
}

fn collect_data_layouts(
    store: &HirStore,
    names: &NameResolution,
    source_id: SourceId,
) -> IrDataLayouts {
    let binding_by_site = binding_by_site(names);
    let mut out = IrDataLayouts::new();

    for (_id, expr) in &store.exprs {
        let HirExprKind::Let { pat, value, .. } = &expr.kind else {
            continue;
        };
        let Some(value) = *value else {
            continue;
        };

        let pat = store.pats.get(*pat);
        let HirPatKind::Bind { name, .. } = &pat.kind else {
            continue;
        };

        let site = NameSite::new(source_id, name.span);
        let Some(binding) = binding_by_site.get(&site).copied() else {
            continue;
        };
        let sym = names.bindings[binding].name;

        let value = store.exprs.get(value);
        let HirExprKind::Data { variants, fields } = &value.kind else {
            continue;
        };

        let layout = IrDataLayout {
            record_fields: fields.as_ref().map(|f| {
                f.iter()
                    .map(|d| d.name.name)
                    .collect::<Vec<_>>()
                    .into_boxed_slice()
            }),
            choice_variants: variants.as_ref().map(|v| {
                v.iter()
                    .map(|d| d.name.name)
                    .collect::<Vec<_>>()
                    .into_boxed_slice()
            }),
        };

        let _prev = out.insert(sym, layout);
    }

    out
}

fn binding_by_site(names: &NameResolution) -> HashMap<NameSite, NameBindingId> {
    let mut out = HashMap::with_capacity(names.bindings.len());
    for (id, binding) in &names.bindings {
        let _prev = out.insert(binding.site, id);
    }
    out
}
