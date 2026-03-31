use std::collections::BTreeMap;

use music_hir::{HirDim, HirExprId, HirExprKind, HirTyKind};
use music_names::Symbol;

use crate::checker::Checker;
use crate::effects::{EffectKey, EffectRow};
use crate::env::{ClassFamily, DataDef, EffectFamily, ValueScheme};
use crate::iface::{
    ExportArrowFlavor, ExportClassFamily, ExportClassOpSig, ExportConstraint, ExportDataDef,
    ExportDim, ExportEffectFamily, ExportEffectKey, ExportEffectOpSig, ExportEffectRow,
    ExportInstance, ExportItem, ExportItemKind, ExportTy, ExportTyBinOp, ExportTyNamed,
    ExportValueScheme, ExportedName, ModuleExportSummary,
};
use crate::{SemTy, SemTyId};

impl Checker<'_> {
    pub(super) fn build_export_summary(&self, root: HirExprId) -> ModuleExportSummary {
        let mut exports = Vec::<ExportedName>::new();
        let mut items = BTreeMap::<String, ExportItem>::new();
        let mut instances = Vec::<ExportInstance>::new();

        let root = self.ctx.store.exprs.get(root);
        let HirExprKind::Sequence { exprs, .. } = &root.kind else {
            return ModuleExportSummary {
                exports: Box::new([]),
                items: BTreeMap::new(),
                instances: Box::new([]),
            };
        };

        for &expr_id in exprs {
            let expr = self.ctx.store.exprs.get(expr_id);
            match &expr.kind {
                HirExprKind::Let {
                    mods, pat, value, ..
                } => {
                    if !mods.exported {
                        continue;
                    }

                    let mut bind_sites = Vec::new();
                    self.collect_bind_sites(*pat, &mut bind_sites);

                    for span in bind_sites {
                        let Some(binding) = self.binding_for_def(span) else {
                            continue;
                        };
                        let name_sym = self.ctx.names.bindings[binding].name;
                        let name = String::from(self.ctx.interner.resolve(name_sym));

                        let scheme = self
                            .state
                            .env
                            .get_value(binding)
                            .cloned()
                            .map_or_else(|| unknown_scheme(), |s| self.export_value_scheme(&s));

                        let mut kind = value
                            .and_then(|v| self.export_item_kind(name_sym, binding, v))
                            .unwrap_or(ExportItemKind::Value);

                        if mods.opaque {
                            if let ExportItemKind::Data { def } = &mut kind {
                                def.variants = None;
                                def.fields = None;
                            }
                        }

                        let opaque = mods.opaque;
                        let item = ExportItem {
                            scheme,
                            kind,
                            opaque,
                        };
                        let _prev = items.insert(name.clone(), item);
                        exports.push(ExportedName { name, opaque });
                    }
                }
                HirExprKind::Instance {
                    mods,
                    ty_params,
                    target,
                    ..
                } => {
                    if !mods.exported {
                        continue;
                    }
                    let target_ty = self.ctx.store.tys.get(*target).clone();
                    let HirTyKind::Named { name, .. } = target_ty.kind else {
                        continue;
                    };

                    let scheme = self.state.flow.instance_schemes.get(&expr_id).cloned();
                    let (args, generic_count, constraints) = match scheme {
                        Some(scheme) => {
                            let args = scheme
                                .args
                                .iter()
                                .copied()
                                .map(|t| self.export_sem_ty(t))
                                .collect::<Vec<_>>()
                                .into_boxed_slice();
                            let constraints = scheme
                                .constraints
                                .iter()
                                .map(|c| self.export_constraint(c))
                                .collect::<Vec<_>>()
                                .into_boxed_slice();
                            (args, scheme.generic_count, constraints)
                        }
                        None => (
                            Vec::new().into_boxed_slice(),
                            u32::try_from(ty_params.len()).unwrap_or(0),
                            Vec::new().into_boxed_slice(),
                        ),
                    };

                    let target = ExportTyNamed {
                        name: String::from(self.ctx.interner.resolve(name.name)),
                        args,
                    };
                    instances.push(ExportInstance {
                        target,
                        generic_count,
                        constraints,
                    });
                }
                _ => {}
            }
        }

        exports.sort_by(|a, b| a.name.cmp(&b.name));
        exports.dedup_by(|a, b| a.name == b.name);
        instances.sort_by(|a, b| a.target.name.cmp(&b.target.name));

        ModuleExportSummary {
            exports: exports.into_boxed_slice(),
            items,
            instances: instances.into_boxed_slice(),
        }
    }

    fn export_item_kind(
        &self,
        name_sym: Symbol,
        binding: music_names::NameBindingId,
        value: HirExprId,
    ) -> Option<ExportItemKind> {
        match &self.ctx.store.exprs.get(value).kind {
            HirExprKind::Data { .. } => {
                self.state
                    .env
                    .get_data_def(name_sym)
                    .cloned()
                    .map(|def| ExportItemKind::Data {
                        def: self.export_data_def(def),
                    })
            }
            HirExprKind::Effect { .. } => {
                self.state
                    .env
                    .get_effect_family(binding)
                    .cloned()
                    .map(|fam| ExportItemKind::Effect {
                        family: self.export_effect_family(fam),
                    })
            }
            HirExprKind::Class { .. } => {
                self.state
                    .env
                    .get_class_family(binding)
                    .cloned()
                    .map(|fam| ExportItemKind::Class {
                        family: self.export_class_family(fam),
                    })
            }
            _ => Some(ExportItemKind::Value),
        }
    }

    fn export_value_scheme(&self, scheme: &ValueScheme) -> ExportValueScheme {
        ExportValueScheme {
            generic_count: scheme.generic_count,
            ty: self.export_sem_ty(scheme.ty),
            declared_effects: scheme
                .declared_effects
                .as_ref()
                .map(|row| self.export_effect_row(row)),
            constraints: scheme
                .constraints
                .iter()
                .map(|c| self.export_constraint(c))
                .collect(),
        }
    }

    fn export_effect_row(&self, row: &EffectRow) -> ExportEffectRow {
        let items = row
            .items
            .iter()
            .cloned()
            .map(|EffectKey { name, arg }| ExportEffectKey {
                name: String::from(self.ctx.interner.resolve(name)),
                arg: arg.map(|t| self.export_sem_ty(t)),
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        ExportEffectRow {
            items,
            is_open: row.is_open,
        }
    }

    fn export_constraint(&self, c: &crate::env::SemConstraint) -> ExportConstraint {
        match c {
            crate::env::SemConstraint::Subtype {
                name, idx, bound, ..
            } => ExportConstraint::Subtype {
                name: String::from(self.ctx.interner.resolve(*name)),
                idx: *idx,
                bound: ExportTyNamed {
                    name: String::from(self.ctx.interner.resolve(bound.name)),
                    args: bound.args.iter().map(|t| self.export_sem_ty(*t)).collect(),
                },
            },
            crate::env::SemConstraint::Implements {
                name, idx, class, ..
            } => ExportConstraint::Implements {
                name: String::from(self.ctx.interner.resolve(*name)),
                idx: *idx,
                class: ExportTyNamed {
                    name: String::from(self.ctx.interner.resolve(class.name)),
                    args: class.args.iter().map(|t| self.export_sem_ty(*t)).collect(),
                },
            },
        }
    }

    fn export_data_def(&self, def: DataDef) -> ExportDataDef {
        let variants = def.variants.map(|vars| {
            let mut out = BTreeMap::<String, Option<ExportTy>>::new();
            for (k, v) in vars {
                let name = String::from(self.ctx.interner.resolve(k));
                let _prev = out.insert(name, v.map(|t| self.export_sem_ty(t)));
            }
            out
        });
        let fields = def.fields.map(|fields| {
            let mut out = BTreeMap::<String, ExportTy>::new();
            for (k, v) in fields {
                let name = String::from(self.ctx.interner.resolve(k));
                let _prev = out.insert(name, self.export_sem_ty(v.ty));
            }
            out
        });
        ExportDataDef {
            generic_count: def.generic_count,
            variants,
            fields,
        }
    }

    fn export_effect_family(&self, fam: EffectFamily) -> ExportEffectFamily {
        let mut ops = BTreeMap::<String, ExportEffectOpSig>::new();
        for (k, sig) in fam.ops {
            let name = String::from(self.ctx.interner.resolve(k));
            let params = sig
                .params
                .iter()
                .map(|t| self.export_sem_ty(*t))
                .collect::<Vec<_>>()
                .into_boxed_slice();
            let ret = self.export_sem_ty(sig.ret);
            let _prev = ops.insert(name, ExportEffectOpSig { params, ret });
        }
        ExportEffectFamily {
            generic_count: fam.generic_count,
            ops,
        }
    }

    fn export_class_family(&self, fam: ClassFamily) -> ExportClassFamily {
        let mut ops = BTreeMap::<String, ExportClassOpSig>::new();
        for (k, sig) in fam.ops {
            let name = String::from(self.ctx.interner.resolve(k));
            let params = sig
                .params
                .iter()
                .map(|t| self.export_sem_ty(*t))
                .collect::<Vec<_>>()
                .into_boxed_slice();
            let ret = self.export_sem_ty(sig.ret);
            let _prev = ops.insert(name, ExportClassOpSig { params, ret });
        }
        ExportClassFamily {
            generic_count: fam.generic_count,
            ops,
        }
    }

    fn export_sem_ty(&self, ty: SemTyId) -> ExportTy {
        let ty = crate::unify::resolve(&self.state.semtys, ty);
        match self.state.semtys.get(ty).clone() {
            SemTy::Error => ExportTy::Error,
            SemTy::Unknown => ExportTy::Unknown,
            SemTy::Any => ExportTy::Any,
            SemTy::InferVar(_) => ExportTy::Unknown,
            SemTy::Generic(i) => ExportTy::Generic(i),
            SemTy::Named { name, args } => ExportTy::Named {
                name: String::from(self.ctx.interner.resolve(name)),
                args: args.iter().map(|t| self.export_sem_ty(*t)).collect(),
            },
            SemTy::Tuple { items } => ExportTy::Tuple {
                items: items.iter().map(|t| self.export_sem_ty(*t)).collect(),
            },
            SemTy::Array { dims, elem } => ExportTy::Array {
                dims: dims
                    .iter()
                    .map(|d| match d {
                        HirDim::Inferred { .. } | HirDim::IntLit { value: None, .. } => {
                            ExportDim::Inferred
                        }
                        HirDim::IntLit { value: Some(v), .. } => ExportDim::Int(*v),
                        HirDim::Name { name } => {
                            ExportDim::Name(String::from(self.ctx.interner.resolve(name.name)))
                        }
                    })
                    .collect(),
                elem: Box::new(self.export_sem_ty(elem)),
            },
            SemTy::Arrow {
                flavor,
                input,
                output,
            } => ExportTy::Arrow {
                flavor: match flavor {
                    music_hir::HirArrowFlavor::Pure => ExportArrowFlavor::Pure,
                    music_hir::HirArrowFlavor::Effectful => ExportArrowFlavor::Effectful,
                },
                input: Box::new(self.export_sem_ty(input)),
                output: Box::new(self.export_sem_ty(output)),
            },
            SemTy::Binary { op, left, right } => ExportTy::Binary {
                op: match op {
                    music_hir::HirTyBinOp::Sum => ExportTyBinOp::Sum,
                    music_hir::HirTyBinOp::Product => ExportTyBinOp::Product,
                },
                left: Box::new(self.export_sem_ty(left)),
                right: Box::new(self.export_sem_ty(right)),
            },
            SemTy::Mut { base } => ExportTy::Mut {
                base: Box::new(self.export_sem_ty(base)),
            },
            SemTy::Record { fields } => {
                let mut out = fields
                    .into_iter()
                    .map(|(k, v)| {
                        (
                            String::from(self.ctx.interner.resolve(k)),
                            self.export_sem_ty(v),
                        )
                    })
                    .collect::<Vec<_>>();
                out.sort_by(|a, b| a.0.cmp(&b.0));
                ExportTy::Record {
                    fields: out.into_boxed_slice(),
                }
            }
        }
    }
}

fn unknown_scheme() -> ExportValueScheme {
    ExportValueScheme {
        generic_count: 0,
        ty: ExportTy::Unknown,
        declared_effects: None,
        constraints: Box::new([]),
    }
}
