use music_ast::common::{Constraint, EffectItem, Param, Signature};
use music_ast::expr::{ExprKind, LetBinding};
use music_ast::pat::PatKind;
use music_ast::ty::TyKind;
use music_ast::{ExprId, TyId};
use music_shared::{Span, Symbol};

use super::SemaDb;
use crate::effects;
use crate::errors::{SemaError, SemaErrorKind};
use crate::types::{SemaTypeId, SemaTypeList, Ty};
use crate::unify::unify;

impl SemaDb {
    pub(super) fn synth_let(&mut self, binding: &LetBinding) -> SemaTypeId {
        let span = binding
            .value
            .map_or_else(|| Span::new(0, 0), |v| self.db.ast.exprs.get(v).span);

        if binding.modifiers.exported && self.depth > 0 {
            self.errors.push(SemaError {
                kind: SemaErrorKind::ExportNotTopLevel,
                span,
                context: None,
            });
        }

        if binding.modifiers.opaque && !binding.modifiers.exported {
            self.errors.push(SemaError {
                kind: SemaErrorKind::OpaqueWithoutExport,
                span,
                context: None,
            });
        }

        if binding.modifiers.foreign && self.depth > 0 {
            self.errors.push(SemaError {
                kind: SemaErrorKind::ForeignNotTopLevel,
                span,
                context: None,
            });
        }

        if binding.modifiers.foreign {
            self.validate_foreign_sig(binding, span);
        }

        if binding.modifiers.mutable {
            let pat = self.db.ast.pats.get(binding.pat);
            if let PatKind::Bind(bind_ident) = &pat.kind {
                if let Some(def_id) = self.find_def_for_name(bind_ident.name) {
                    let _inserted = self.mutable_defs.insert(def_id);
                }
            }
        }

        self.maybe_assign_class_id(binding);

        let val_ty = binding.value.map(|value| self.synth(value));

        if let Some(ref sig) = binding.sig {
            self.process_let_sig(binding, sig, val_ty);
        }

        if let Some(vt) = val_ty {
            if let Some(value_id) = binding.value {
                let pat = self.db.ast.pats.get(binding.pat);
                if let PatKind::Bind(bind_ident) = &pat.kind {
                    let ty = self.env.type_map.get(&value_id).copied().unwrap_or(vt);
                    let _prev = self.let_types.insert(bind_ident.name, ty);
                }
            }
        }

        self.env.intern(Ty::Unit)
    }

    pub(super) fn process_let_sig(
        &mut self,
        binding: &LetBinding,
        sig: &Signature,
        val_ty: Option<SemaTypeId>,
    ) {
        if !sig.constraints.is_empty() {
            let pat = self.db.ast.pats.get(binding.pat);
            if let PatKind::Bind(bind_ident) = &pat.kind {
                if let Some(def_id) = self.find_def_for_name(bind_ident.name) {
                    let constraints: Vec<(Symbol, Symbol)> = sig
                        .constraints
                        .iter()
                        .filter_map(|c| match c {
                            Constraint::Implements { ty, class } => {
                                Some((ty.name, class.name.name))
                            }
                            Constraint::Subtype { .. } => None,
                        })
                        .collect();
                    if !constraints.is_empty() {
                        let _prev = self.def_constraints.insert(def_id, constraints);
                    }
                }
            }
        }

        if let Some(ret_ty_id) = sig.ret_ty {
            let expected = self.lower_ty(ret_ty_id);
            if let Some(vt) = val_ty {
                if let Some(value_id) = binding.value {
                    let span = self.db.ast.exprs.get(value_id).span;
                    if let Err(e) = unify(&mut self.env, vt, expected, span) {
                        self.errors.push(e);
                    }
                }
            }
        }

        if sig.has_param_list {
            let param_ty = if sig.params.len() == 1 {
                match sig.params[0].ty {
                    Some(t) => self.lower_ty(t),
                    None => self.env.fresh_var(),
                }
            } else {
                let pts: SemaTypeList = sig
                    .params
                    .iter()
                    .map(|p| match p.ty {
                        Some(t) => self.lower_ty(t),
                        None => self.env.fresh_var(),
                    })
                    .collect();
                self.env.intern(Ty::Tuple(pts))
            };
            let ret_ty = match sig.ret_ty {
                Some(t) => self.lower_ty(t),
                None => val_ty.unwrap_or_else(|| self.env.fresh_var()),
            };
            let declared_effects = self.lower_signature_effects(sig);
            let arrow = if declared_effects.is_empty()
                && sig.ret_ty.is_none_or(|t| !self.is_effect_arrow_context(t))
            {
                self.env.intern(Ty::Arrow {
                    param: param_ty,
                    ret: ret_ty,
                })
            } else {
                self.env.intern(Ty::EffectArrow {
                    param: param_ty,
                    ret: ret_ty,
                    effects: declared_effects,
                })
            };
            if let Some(value_id) = binding.value {
                let _ = self.env.type_map.insert(value_id, arrow);
            }
        }
    }

    pub(super) fn lower_signature_effects(&mut self, sig: &Signature) -> SemaTypeList {
        sig.effects
            .iter()
            .filter_map(|item| match item {
                EffectItem::Named { name, .. } => Some(self.env.intern(Ty::Effect(name.name))),
                EffectItem::Rest(_) => None,
            })
            .collect()
    }

    pub(super) fn validate_foreign_sig(&mut self, binding: &LetBinding, span: Span) {
        let Some(ref sig) = binding.sig else { return };
        for param in &sig.params {
            if let Some(ty_id) = param.ty {
                if let Some(type_name) = self.non_ffi_type_name(ty_id) {
                    self.errors.push(SemaError {
                        kind: SemaErrorKind::IncompatibleFfiType { type_name },
                        span,
                        context: None,
                    });
                }
            }
        }
        if let Some(ret_ty_id) = sig.ret_ty {
            if let Some(type_name) = self.non_ffi_type_name(ret_ty_id) {
                self.errors.push(SemaError {
                    kind: SemaErrorKind::IncompatibleFfiType { type_name },
                    span,
                    context: None,
                });
            }
        }
    }

    pub(super) fn non_ffi_type_name(&self, ty_id: TyId) -> Option<String> {
        let ty_kind = &self.db.ast.types.get(ty_id).kind;
        if let TyKind::Named { name, .. } = ty_kind {
            let resolved = self.db.interner.resolve(name.name);
            match resolved {
                "Int" | "Int8" | "Int16" | "Int32" | "Int64" | "Nat" | "Nat8" | "Nat16"
                | "Nat32" | "Nat64" | "Float" | "Float32" | "Float64" | "Bool" | "Unit"
                | "Rune" | "String" | "CPtr" | "CString" => None,
                _ => Some(resolved.to_owned()),
            }
        } else {
            Some(format!("{ty_kind:?}"))
        }
    }

    pub(super) fn maybe_assign_class_id(&mut self, binding: &LetBinding) {
        if let Some(value_id) = binding.value {
            let val_kind = &self.db.ast.exprs.get(value_id).kind;
            if matches!(val_kind, ExprKind::ClassDef(_)) {
                let pat = self.db.ast.pats.get(binding.pat);
                if let PatKind::Bind(bind_ident) = &pat.kind {
                    let _ = self.env.assign_class_id(bind_ident.name);
                }
            }
        }
    }

    pub(super) fn synth_lambda(
        &mut self,
        params: &[Param],
        ret_ty: Option<TyId>,
        body: ExprId,
    ) -> SemaTypeId {
        let param_ty = if params.is_empty() {
            self.env.intern(Ty::Unit)
        } else if params.len() == 1 {
            self.env.fresh_var()
        } else {
            let param_tys: SemaTypeList = params.iter().map(|_| self.env.fresh_var()).collect();
            self.env.intern(Ty::Tuple(param_tys))
        };

        self.depth = self.depth.saturating_add(1);
        let body_ty = self.synth(body);
        self.depth = self.depth.saturating_sub(1);

        let is_declared_pure = ret_ty.is_none_or(|ty_id| !self.is_effect_arrow_context(ty_id));
        let body_effects = self.collect_need_exprs(body);
        let arrow_ty = if is_declared_pure {
            self.env.intern(Ty::Arrow {
                param: param_ty,
                ret: body_ty,
            })
        } else {
            self.env.intern(Ty::EffectArrow {
                param: param_ty,
                ret: body_ty,
                effects: body_effects.clone(),
            })
        };

        if is_declared_pure {
            if let Some(effect_ty) = effects::check_purity(&self.env, arrow_ty, &body_effects) {
                let effect_sym = self.effect_symbol(effect_ty);
                let span = self.db.ast.exprs.get(body).span;
                self.errors.push(SemaError {
                    kind: SemaErrorKind::PurityViolation { effect: effect_sym },
                    span,
                    context: None,
                });
            }
        }

        arrow_ty
    }

    pub(super) fn is_effect_arrow_context(&self, ty_id: TyId) -> bool {
        let kind = &self.db.ast.types.get(ty_id).kind;
        matches!(kind, TyKind::EffectArrow { .. })
    }

    pub(super) fn collect_need_exprs(&self, expr_id: ExprId) -> SemaTypeList {
        let mut effects = Vec::new();
        self.walk_for_needs(expr_id, &mut effects);
        effects
    }

    pub(super) fn walk_for_needs(&self, expr_id: ExprId, out: &mut SemaTypeList) {
        let kind = &self.db.ast.exprs.get(expr_id).kind;
        match kind {
            ExprKind::Perform(_) => {
                if let Some(effect_tys) = self.env.effect_map.get(&expr_id) {
                    out.extend(effect_tys.iter().copied());
                }
            }
            ExprKind::Seq(stmts) => {
                for &s in stmts {
                    self.walk_for_needs(s, out);
                }
            }
            ExprKind::Let(binding) => {
                if let Some(v) = binding.value {
                    self.walk_for_needs(v, out);
                }
            }
            ExprKind::Branch {
                cond,
                then_br,
                else_br,
            } => {
                self.walk_for_needs(*cond, out);
                self.walk_for_needs(*then_br, out);
                self.walk_for_needs(*else_br, out);
            }
            ExprKind::App(callee, args) => {
                self.walk_for_needs(*callee, out);
                for &a in args {
                    self.walk_for_needs(a, out);
                }
            }
            ExprKind::BinOp(_, lhs, rhs) | ExprKind::Assign(lhs, rhs) => {
                self.walk_for_needs(*lhs, out);
                self.walk_for_needs(*rhs, out);
            }
            ExprKind::UnaryOp(_, operand)
            | ExprKind::Postfix { expr: operand, .. }
            | ExprKind::Access { expr: operand, .. }
            | ExprKind::Return(Some(operand))
            | ExprKind::Resume(Some(operand)) => {
                self.walk_for_needs(*operand, out);
            }
            ExprKind::Case(data) => {
                self.walk_for_needs(data.scrutinee, out);
                for arm in &data.arms {
                    self.walk_for_needs(arm.body, out);
                }
            }
            _ => {}
        }
    }

    pub(super) fn effect_symbol(&mut self, ty_id: SemaTypeId) -> Symbol {
        let resolved = self.env.resolve_var(ty_id);
        let ty = self.env.types.get(resolved);
        match ty {
            Ty::Effect(sym) | Ty::Class(sym) | Ty::Param(sym) => *sym,
            Ty::Named(name) => name.name,
            _ => self.db.interner.intern("_"),
        }
    }
}
