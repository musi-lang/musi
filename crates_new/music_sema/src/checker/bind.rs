use std::collections::HashMap;

use music_basic::Span;
use music_hir::{HirPatId, HirPatKind};
use music_names::Symbol;

use super::{SemTyId};
use super::check::Checker;
use super::env::{EffectOpSig, ValueScheme};

impl<'a> Checker<'a> {
    pub(super) fn bind_pat_to_scheme(&mut self, pat: HirPatId, scheme: ValueScheme) {
        let mut stack = vec![pat];
        while let Some(pat_id) = stack.pop() {
            let pat = self.ctx.store.pats.get(pat_id).clone();
            match pat.kind {
                HirPatKind::Bind { name, sub } => {
                    if let Some(binding) = self.binding_for_def(name.span) {
                        self.state.env.insert_value(binding, scheme.clone());
                    }
                    if let Some(sub) = sub {
                        stack.push(sub);
                    }
                }
                HirPatKind::Variant { args, .. }
                | HirPatKind::Tuple { items: args }
                | HirPatKind::Array { items: args }
                | HirPatKind::Or { alts: args } => stack.extend(args.iter().copied()),
                HirPatKind::Record { fields } => {
                    for f in fields.iter() {
                        if f.sub.is_none() {
                            if let Some(binding) = self.binding_for_def(f.name.span) {
                                self.state.env.insert_value(binding, scheme.clone());
                            }
                        }
                        if let Some(sub) = f.sub {
                            stack.push(sub);
                        }
                    }
                }
                HirPatKind::Wildcard | HirPatKind::Lit { .. } | HirPatKind::Error => {}
            }
        }
    }

    pub(super) fn bind_pat_for_scrut(&mut self, pat: HirPatId, scrut_ty: SemTyId) {
        let pat = self.ctx.store.pats.get(pat).clone();
        match pat.kind {
            HirPatKind::Bind { name, sub } => {
                if let Some(binding) = self.binding_for_def(name.span) {
                    self.state.env.insert_value(
                        binding,
                        ValueScheme {
                            generic_count: 0,
                            ty: scrut_ty,
                            declared_effects: None,
                        },
                    );
                }
                if let Some(sub) = sub {
                    self.bind_pat_for_scrut(sub, scrut_ty);
                }
            }
            HirPatKind::Tuple { items } | HirPatKind::Array { items } => {
                for p in items.iter().copied() {
                    self.bind_pat_for_scrut(p, self.state.builtins.unknown);
                }
            }
            HirPatKind::Record { fields } => {
                for f in fields.iter() {
                    if f.sub.is_none() {
                        if let Some(binding) = self.binding_for_def(f.name.span) {
                            self.state.env.insert_value(
                                binding,
                                ValueScheme {
                                    generic_count: 0,
                                    ty: self.state.builtins.unknown,
                                    declared_effects: None,
                                },
                            );
                        }
                    }
                    if let Some(sub) = f.sub {
                        self.bind_pat_for_scrut(sub, self.state.builtins.unknown);
                    }
                }
            }
            HirPatKind::Or { alts } => {
                for p in alts.iter().copied() {
                    self.bind_pat_for_scrut(p, scrut_ty);
                }
            }
            HirPatKind::Variant { args, .. } => {
                for p in args.iter().copied() {
                    self.bind_pat_for_scrut(p, self.state.builtins.unknown);
                }
            }
            HirPatKind::Wildcard | HirPatKind::Lit { .. } | HirPatKind::Error => {}
        }
    }

    pub(super) fn bind_value_clause_binder(&mut self, binder: music_names::Ident, ty: SemTyId) {
        if let Some(binding) = self.binding_for_def(binder.span) {
            self.state.env.insert_value(
                binding,
                ValueScheme {
                    generic_count: 0,
                    ty,
                    declared_effects: None,
                },
            );
        }
    }

    pub(super) fn bind_handle_param(&mut self, param: music_names::Ident, ty: SemTyId) {
        if let Some(binding) = self.binding_for_def(param.span) {
            self.state.env.insert_value(
                binding,
                ValueScheme {
                    generic_count: 0,
                    ty,
                    declared_effects: None,
                },
            );
        }
    }

    pub(super) fn register_effect_def(
        &mut self,
        pat: HirPatId,
        ty_params: &HashMap<Symbol, u32>,
        members: &Box<[music_hir::HirMemberDef]>,
    ) {
        // Only supports `let EffectName := effect { ... }` bindings for now.
        let mut bind_sites = Vec::new();
        self.collect_bind_sites(pat, &mut bind_sites);
        let Some(first_bind_span) = bind_sites.first().copied() else {
            return;
        };

        let Some(binding) = self.binding_for_def(first_bind_span) else {
            return;
        };

        let mut ops = HashMap::<Symbol, EffectOpSig>::new();
        for member in members.iter() {
            let music_hir::HirMemberDef::Let { name, params, ret, .. } = member else {
                continue;
            };
            let mut param_tys = Vec::new();
            for p in params.iter() {
                let ty = p
                    .annot
                    .map(|t| self.lower_hir_ty(t, ty_params))
                    .unwrap_or(self.state.builtins.unknown);
                param_tys.push(ty);
            }
            let ret = ret
                .map(|t| self.lower_hir_ty(t, ty_params))
                .unwrap_or(self.state.builtins.unknown);
            let _prev = ops.insert(
                name.name.name,
                EffectOpSig {
                    params: param_tys.into_boxed_slice(),
                    ret,
                },
            );
        }
        self.state.env.insert_effect_ops(binding, ops);
    }

    fn collect_bind_sites(&self, pat: HirPatId, out: &mut Vec<Span>) {
        let mut stack = vec![pat];
        while let Some(pat_id) = stack.pop() {
            let pat = self.ctx.store.pats.get(pat_id);
            match &pat.kind {
                HirPatKind::Bind { name, sub } => {
                    out.push(name.span);
                    if let Some(sub) = sub {
                        stack.push(*sub);
                    }
                }
                HirPatKind::Variant { args, .. }
                | HirPatKind::Tuple { items: args }
                | HirPatKind::Array { items: args }
                | HirPatKind::Or { alts: args } => stack.extend(args.iter().copied()),
                HirPatKind::Record { fields } => {
                    for f in fields.iter() {
                        if let Some(sub) = f.sub {
                            stack.push(sub);
                        } else {
                            out.push(f.name.span);
                        }
                    }
                }
                HirPatKind::Wildcard | HirPatKind::Lit { .. } | HirPatKind::Error => {}
            }
        }
    }
}

