//! Lowering from typed HIR into codegen-facing IR facts.
//!
//! This crate owns `music_ir::IrModuleInfo` construction from post-check HIR.

use std::collections::{HashMap, HashSet};

use music_basic::SourceId;
use music_hir::{
    HirExpr, HirExprId, HirExprKind, HirModule, HirPatKind, HirRecordItem, HirStore, HirTyKind,
};
use music_ir::{IrDataLayout, IrDataLayouts, IrExprTy, IrModuleInfo, IrScalarTy};
use music_known::KnownSymbols;
use music_names::{Ident, Interner, NameBindingId, NameResolution, NameSite, Symbol};

#[must_use]
pub fn build_ir_module_info(
    known: KnownSymbols,
    interner: &Interner,
    module: &HirModule,
    names: &NameResolution,
) -> IrModuleInfo {
    let let_value_by_binding = collect_let_values(&module.store, names, module.source_id);

    let mut lower = IrLower {
        known,
        interner,
        store: &module.store,
        source_id: module.source_id,
        names,
        let_value_by_binding,
        binding_cache: HashMap::new(),
        binding_in_progress: HashSet::new(),
        cache: vec![None; module.store.exprs.len()],
    };

    let mut out = vec![IrExprTy::Unknown; module.store.exprs.len()];
    for (expr_id, _expr) in &module.store.exprs {
        let idx = usize::try_from(expr_id.raw()).unwrap_or(usize::MAX);
        if idx >= out.len() {
            continue;
        }
        out[idx] = lower.ir_expr_ty(expr_id);
    }

    let data_layouts = collect_data_layouts(&module.store, names, module.source_id);

    IrModuleInfo {
        expr_tys: out.into_boxed_slice(),
        data_layouts,
    }
}

struct IrLower<'a> {
    known: KnownSymbols,
    interner: &'a Interner,
    store: &'a HirStore,
    source_id: SourceId,
    names: &'a NameResolution,
    let_value_by_binding: HashMap<NameBindingId, HirExprId>,
    binding_cache: HashMap<NameBindingId, IrExprTy>,
    binding_in_progress: HashSet<NameBindingId>,
    cache: Vec<Option<IrExprTy>>,
}

impl<'a> IrLower<'a> {
    fn ir_expr_ty(&mut self, expr_id: HirExprId) -> IrExprTy {
        let idx = usize::try_from(expr_id.raw()).unwrap_or(usize::MAX);
        if idx >= self.cache.len() {
            return IrExprTy::Unknown;
        }
        if let Some(ty) = self.cache[idx].clone() {
            return ty;
        }

        let expr = self.store.exprs.get(expr_id).clone();
        let ty = self.lower_expr(&expr);
        self.cache[idx] = Some(ty.clone());
        ty
    }

    fn lower_expr(&mut self, expr: &HirExpr) -> IrExprTy {
        match &expr.kind {
            HirExprKind::Named { ident } => self.named_expr_ty(*ident, expr.ty),
            HirExprKind::Record { items } => self.record_expr_ty(items.as_ref()),
            HirExprKind::Import { exports, .. } => self.import_expr_ty(exports.as_ref()),
            HirExprKind::RecordUpdate { base, .. } => match self.ir_expr_ty(*base) {
                IrExprTy::Record { fields } => IrExprTy::Record { fields },
                IrExprTy::Named(sym) => IrExprTy::Named(sym),
                other => other,
            },
            _ => self.lower_hir_ty(expr.ty),
        }
    }

    fn named_expr_ty(&mut self, ident: Ident, fallback: music_hir::HirTyId) -> IrExprTy {
        let site = NameSite::new(self.source_id, ident.span);
        let Some(binding) = self.names.refs.get(&site).copied() else {
            return self.lower_hir_ty(fallback);
        };
        if let Some(ty) = self.binding_cache.get(&binding).cloned() {
            return ty;
        }
        let Some(value_expr) = self.let_value_by_binding.get(&binding).copied() else {
            return self.lower_hir_ty(fallback);
        };
        if self.binding_in_progress.contains(&binding) {
            return IrExprTy::Unknown;
        }
        let _did_insert = self.binding_in_progress.insert(binding);
        let ty = self.ir_expr_ty(value_expr);
        let _did_remove = self.binding_in_progress.remove(&binding);
        let _prev = self.binding_cache.insert(binding, ty.clone());
        ty
    }

    fn record_expr_ty(&self, items: &[HirRecordItem]) -> IrExprTy {
        if items
            .iter()
            .any(|it| matches!(it, HirRecordItem::Spread { .. }))
        {
            return IrExprTy::Unknown;
        }

        let mut set = HashSet::<Symbol>::new();
        for it in items {
            let HirRecordItem::Field { name, .. } = it else {
                continue;
            };
            let _did_insert = set.insert(name.name);
        }

        let mut fields = set.into_iter().collect::<Vec<_>>();
        fields.sort_by(|a, b| self.interner.resolve(*a).cmp(self.interner.resolve(*b)));
        IrExprTy::Record {
            fields: fields.into_boxed_slice(),
        }
    }

    fn import_expr_ty(&self, exports: &[Symbol]) -> IrExprTy {
        let mut fields = exports.to_vec();
        fields.sort_by(|a, b| self.interner.resolve(*a).cmp(self.interner.resolve(*b)));
        fields.dedup();
        IrExprTy::Record {
            fields: fields.into_boxed_slice(),
        }
    }

    fn lower_hir_ty(&self, ty_id: music_hir::HirTyId) -> IrExprTy {
        let ty = self.store.tys.get(ty_id);
        match &ty.kind {
            HirTyKind::Error => IrExprTy::Error,
            HirTyKind::Named { name, args } => {
                if args.is_empty() {
                    match name.name {
                        s if s == self.known.unit => return IrExprTy::Scalar(IrScalarTy::Unit),
                        s if s == self.known.bool_ => return IrExprTy::Scalar(IrScalarTy::Bool),
                        s if s == self.known.int_ => return IrExprTy::Scalar(IrScalarTy::Int),
                        s if s == self.known.float_ => return IrExprTy::Scalar(IrScalarTy::Float),
                        s if s == self.known.string_ => {
                            return IrExprTy::Scalar(IrScalarTy::String);
                        }
                        s if s == self.known.any => return IrExprTy::Any,
                        s if s == self.known.unknown => return IrExprTy::Unknown,
                        _ => {}
                    }
                }
                IrExprTy::Named(name.name)
            }
            HirTyKind::Tuple { items } => IrExprTy::Tuple {
                arity: u16::try_from(items.len()).unwrap_or(u16::MAX),
            },
            HirTyKind::Array { elem, .. } => IrExprTy::Array {
                elem: self.lower_hir_ty(*elem).as_ty_ref(),
            },
            HirTyKind::Arrow { .. }
            | HirTyKind::Binary { .. }
            | HirTyKind::Mut { .. }
            | HirTyKind::Pi { .. } => IrExprTy::Unknown,
        }
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

fn collect_let_values(
    store: &HirStore,
    names: &NameResolution,
    source_id: SourceId,
) -> HashMap<NameBindingId, HirExprId> {
    let binding_by_site = binding_by_site(names);
    let mut out = HashMap::<NameBindingId, HirExprId>::new();

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

        let _prev = out.insert(binding, value);
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
