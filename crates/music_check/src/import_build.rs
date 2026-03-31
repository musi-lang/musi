use std::collections::{BTreeMap, HashMap};
use std::path::Path;

use music_basic::{Span, path};
use music_hir::{HirArrowFlavor, HirDim, HirPatId, HirPatKind, HirTyBinOp};
use music_names::{Ident, Symbol};

use crate::checker::{Checker, ImportRecord, ImportRecordEntry};
use crate::env::{
    ClassFamily, ClassOpSig, DataDef, DataFieldDef, EffectFamily, EffectOpSig, InstanceScheme,
    SemConstraint, SemTyNamed, ValueScheme,
};
use crate::iface::{
    ExportArrowFlavor, ExportClassFamily, ExportConstraint, ExportDataDef, ExportDim,
    ExportEffectFamily, ExportEffectRow, ExportInstance, ExportItemKind, ExportTy, ExportTyBinOp,
    ExportTyNamed, ExportValueScheme, ModuleExportSummary,
};
use crate::{EffectKey, EffectRow, SemTy, SemTyId};

impl Checker<'_> {
    pub(crate) fn normalize_import_key(&self, raw: &str) -> String {
        if raw.starts_with('@') {
            return raw.to_owned();
        }
        let path = Path::new(raw);
        if !raw.starts_with('.') && !path.is_absolute() {
            return raw.to_owned();
        }

        let Some(source) = self.ctx.sources.get(self.ctx.source_id) else {
            return raw.to_owned();
        };
        path::resolve_import_path(source.path(), raw)
            .to_string_lossy()
            .into_owned()
    }

    pub(crate) fn import_module_semantics(
        &mut self,
        module_key: &str,
        summary: &ModuleExportSummary,
    ) {
        if self.state.imported_modules.contains(module_key) {
            return;
        }
        let _did_insert = self.state.imported_modules.insert(String::from(module_key));

        for export in summary.exports.iter() {
            if !export.opaque {
                continue;
            }
            let sym = self.ctx.interner.intern(export.name.as_str());
            let _did_insert = self.state.opaque_imports.insert(sym);
        }

        for (name, item) in summary.items.iter() {
            let sym = self.ctx.interner.intern(name.as_str());
            match &item.kind {
                ExportItemKind::Data { def } => {
                    if item.opaque {
                        continue;
                    }
                    let def = self.import_data_def(def);
                    self.state.env.insert_data_def(sym, def);
                }
                _ => {}
            }
        }

        for inst in summary.instances.iter() {
            let Some((class, scheme)) = self.import_instance_scheme(inst) else {
                continue;
            };
            self.state.env.insert_instance(class, scheme);
        }
    }

    pub(crate) fn build_import_record(&mut self, summary: &ModuleExportSummary) -> ImportRecord {
        let mut entries = HashMap::<Symbol, ImportRecordEntry>::new();

        for (name, item) in summary.items.iter() {
            let sym = self.ctx.interner.intern(name.as_str());
            let scheme = self.import_value_scheme(&item.scheme);

            let mut effect_family = None;
            let mut class_family = None;
            if !item.opaque {
                match &item.kind {
                    ExportItemKind::Effect { family } => {
                        effect_family = Some(self.import_effect_family(family));
                    }
                    ExportItemKind::Class { family } => {
                        class_family = Some(self.import_class_family(family));
                    }
                    _ => {}
                }
            }

            let _prev = entries.insert(
                sym,
                ImportRecordEntry {
                    scheme,
                    effect_family,
                    class_family,
                },
            );
        }

        ImportRecord { entries }
    }

    pub(crate) fn import_opened_bindings(
        &mut self,
        import_span: Span,
        exports: &[Symbol],
        record: &ImportRecord,
    ) {
        for &sym in exports {
            let Some(binding) = self
                .ctx
                .import_binding_by_key
                .get(&(import_span, sym))
                .copied()
            else {
                continue;
            };
            let Some(entry) = record.entries.get(&sym).cloned() else {
                continue;
            };
            self.state.env.insert_value(binding, entry.scheme.clone());
            if let Some(fam) = entry.effect_family.as_ref() {
                self.state
                    .env
                    .insert_effect_family(binding, fam.generic_count, fam.ops.clone());
            }
            if let Some(fam) = entry.class_family.as_ref() {
                self.state
                    .env
                    .insert_class_family(binding, fam.generic_count, fam.ops.clone());
            }
        }
    }

    pub(crate) fn bind_import_record_pat(&mut self, pat: HirPatId, record: &ImportRecord) {
        let pat = self.ctx.store.pats.get(pat).clone();
        let HirPatKind::Record { fields } = pat.kind else {
            return;
        };

        for f in fields.iter() {
            let entry = record.entries.get(&f.name.name);
            let scheme = entry.map(|e| e.scheme.clone()).unwrap_or(ValueScheme {
                generic_count: 0,
                ty: self.state.builtins.unknown,
                declared_effects: None,
                constraints: Box::new([]),
            });

            if f.mutable {
                if let Some(sub) = f.sub {
                    self.mark_pat_bindings_mut(sub, true);
                } else if let Some(binding) = self.binding_for_def(f.name.span) {
                    self.mark_binding_mut(binding, true);
                }
            }

            if f.sub.is_none() {
                if let Some(binding) = self.binding_for_def(f.name.span) {
                    self.state.env.insert_value(binding, scheme.clone());
                    if let Some(entry) = entry {
                        self.bind_import_families(binding, entry);
                    }
                }
            }

            if let Some(sub) = f.sub {
                self.bind_pat_to_scheme(sub, &scheme);
                if let Some(entry) = entry {
                    self.bind_import_families_for_pat(sub, entry);
                }
            }
        }
    }

    pub(crate) fn import_value_scheme(&mut self, scheme: &ExportValueScheme) -> ValueScheme {
        ValueScheme {
            generic_count: scheme.generic_count,
            ty: self.import_ty(&scheme.ty),
            declared_effects: scheme
                .declared_effects
                .as_ref()
                .map(|row| self.import_effect_row(row)),
            constraints: scheme
                .constraints
                .iter()
                .map(|c| self.import_constraint(c))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        }
    }

    fn import_constraint(&mut self, c: &ExportConstraint) -> SemConstraint {
        match c {
            ExportConstraint::Subtype { name, idx, bound } => SemConstraint::Subtype {
                name: self.ctx.interner.intern(name.as_str()),
                idx: *idx,
                bound: self.import_named_ty(bound),
            },
            ExportConstraint::Implements { name, idx, class } => SemConstraint::Implements {
                name: self.ctx.interner.intern(name.as_str()),
                idx: *idx,
                class: self.import_named_ty(class),
            },
        }
    }

    fn import_named_ty(&mut self, ty: &ExportTyNamed) -> SemTyNamed {
        SemTyNamed {
            name: self.ctx.interner.intern(ty.name.as_str()),
            args: ty
                .args
                .iter()
                .map(|t| self.import_ty(t))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        }
    }

    fn import_instance_scheme(
        &mut self,
        inst: &ExportInstance,
    ) -> Option<(Symbol, InstanceScheme)> {
        let class = self.ctx.interner.intern(inst.target.name.as_str());
        let args = inst
            .target
            .args
            .iter()
            .map(|t| self.import_ty(t))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let constraints = inst
            .constraints
            .iter()
            .map(|c| self.import_constraint(c))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        Some((
            class,
            InstanceScheme {
                generic_count: inst.generic_count,
                args,
                constraints,
            },
        ))
    }

    fn import_effect_row(&mut self, row: &ExportEffectRow) -> EffectRow {
        let mut out = EffectRow::empty();
        out.is_open = row.is_open;
        for k in row.items.iter() {
            let name = self.ctx.interner.intern(k.name.as_str());
            let arg = k.arg.as_ref().map(|t| self.import_ty(t));
            out.add(EffectKey { name, arg });
        }
        out
    }

    fn import_data_def(&mut self, def: &ExportDataDef) -> DataDef {
        let variants = def.variants.as_ref().map(|vars| {
            let mut out = HashMap::<Symbol, Option<SemTyId>>::new();
            for (k, v) in vars.iter() {
                let sym = self.ctx.interner.intern(k.as_str());
                let _prev = out.insert(sym, v.as_ref().map(|t| self.import_ty(t)));
            }
            out
        });

        let fields = def.fields.as_ref().map(|fields| {
            let mut out = HashMap::<Symbol, DataFieldDef>::new();
            for (k, v) in fields.iter() {
                let sym = self.ctx.interner.intern(k.as_str());
                let _prev = out.insert(
                    sym,
                    DataFieldDef {
                        ty: self.import_ty(v),
                    },
                );
            }
            out
        });

        DataDef {
            generic_count: def.generic_count,
            variants,
            fields,
        }
    }

    fn import_effect_family(&mut self, fam: &ExportEffectFamily) -> EffectFamily {
        let mut ops = HashMap::<Symbol, EffectOpSig>::new();
        for (k, sig) in fam.ops.iter() {
            let sym = self.ctx.interner.intern(k.as_str());
            let params = sig
                .params
                .iter()
                .map(|t| self.import_ty(t))
                .collect::<Vec<_>>()
                .into_boxed_slice();
            let ret = self.import_ty(&sig.ret);
            let _prev = ops.insert(sym, EffectOpSig { params, ret });
        }
        EffectFamily {
            generic_count: fam.generic_count,
            ops,
        }
    }

    fn import_class_family(&mut self, fam: &ExportClassFamily) -> ClassFamily {
        let mut ops = HashMap::<Symbol, ClassOpSig>::new();
        for (k, sig) in fam.ops.iter() {
            let sym = self.ctx.interner.intern(k.as_str());
            let params = sig
                .params
                .iter()
                .map(|t| self.import_ty(t))
                .collect::<Vec<_>>()
                .into_boxed_slice();
            let ret = self.import_ty(&sig.ret);
            let _prev = ops.insert(sym, ClassOpSig { params, ret });
        }
        ClassFamily {
            generic_count: fam.generic_count,
            ops,
        }
    }

    pub(crate) fn import_ty(&mut self, ty: &ExportTy) -> SemTyId {
        match ty {
            ExportTy::Error => self.state.builtins.error,
            ExportTy::Unknown => self.state.builtins.unknown,
            ExportTy::Any => self.state.builtins.any,
            ExportTy::Generic(i) => self.state.semtys.alloc(SemTy::Generic(*i)),
            ExportTy::Named { name, args } => {
                let sym = self.ctx.interner.intern(name.as_str());
                let args = args
                    .iter()
                    .map(|t| self.import_ty(t))
                    .collect::<Vec<_>>()
                    .into_boxed_slice();
                self.state.semtys.alloc(SemTy::Named { name: sym, args })
            }
            ExportTy::Tuple { items } => {
                let items = items
                    .iter()
                    .map(|t| self.import_ty(t))
                    .collect::<Vec<_>>()
                    .into_boxed_slice();
                self.state.semtys.alloc(SemTy::Tuple { items })
            }
            ExportTy::Array { dims, elem } => {
                let dims = dims
                    .iter()
                    .cloned()
                    .map(|d| self.import_dim(d))
                    .collect::<Vec<_>>()
                    .into_boxed_slice();
                let elem = self.import_ty(elem.as_ref());
                self.state.semtys.alloc(SemTy::Array { dims, elem })
            }
            ExportTy::Arrow {
                flavor,
                input,
                output,
            } => {
                let flavor = match flavor {
                    ExportArrowFlavor::Pure => HirArrowFlavor::Pure,
                    ExportArrowFlavor::Effectful => HirArrowFlavor::Effectful,
                };
                let input = self.import_ty(input.as_ref());
                let output = self.import_ty(output.as_ref());
                self.state.semtys.alloc(SemTy::Arrow {
                    flavor,
                    input,
                    output,
                })
            }
            ExportTy::Binary { op, left, right } => {
                let op = match op {
                    ExportTyBinOp::Sum => HirTyBinOp::Sum,
                    ExportTyBinOp::Product => HirTyBinOp::Product,
                };
                let left = self.import_ty(left.as_ref());
                let right = self.import_ty(right.as_ref());
                self.state.semtys.alloc(SemTy::Binary { op, left, right })
            }
            ExportTy::Mut { base } => {
                let base = self.import_ty(base.as_ref());
                self.state.semtys.alloc(SemTy::Mut { base })
            }
            ExportTy::Record { fields } => {
                let mut out = BTreeMap::<Symbol, SemTyId>::new();
                for (k, v) in fields.iter() {
                    let sym = self.ctx.interner.intern(k.as_str());
                    let _prev = out.insert(sym, self.import_ty(v));
                }
                self.state.semtys.alloc(SemTy::Record { fields: out })
            }
        }
    }

    fn import_dim(&mut self, dim: ExportDim) -> HirDim {
        match dim {
            ExportDim::Inferred => HirDim::Inferred { span: Span::DUMMY },
            ExportDim::Int(v) => HirDim::IntLit {
                span: Span::DUMMY,
                value: Some(v),
            },
            ExportDim::Name(name) => {
                let sym = self.ctx.interner.intern(name.as_str());
                HirDim::Name {
                    name: Ident::new(sym, Span::DUMMY),
                }
            }
        }
    }
}

impl Checker<'_> {
    fn bind_import_families(
        &mut self,
        binding: music_names::NameBindingId,
        entry: &ImportRecordEntry,
    ) {
        if let Some(fam) = entry.effect_family.as_ref() {
            self.state
                .env
                .insert_effect_family(binding, fam.generic_count, fam.ops.clone());
        }
        if let Some(fam) = entry.class_family.as_ref() {
            self.state
                .env
                .insert_class_family(binding, fam.generic_count, fam.ops.clone());
        }
    }

    fn bind_import_families_for_pat(&mut self, pat: HirPatId, entry: &ImportRecordEntry) {
        let mut sites = Vec::new();
        self.collect_bind_sites(pat, &mut sites);
        for span in sites {
            let Some(binding) = self.binding_for_def(span) else {
                continue;
            };
            self.bind_import_families(binding, entry);
        }
    }
}
