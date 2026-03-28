use std::collections::HashSet;

use music_ast::ExprId;
use music_ast::common::TyRef;
use music_ast::expr::{ExprKind, FieldTarget, HandlerClause};
use music_owned::types::BuiltinType;
use music_shared::Span;

use super::SemaDb;
use crate::env::EffectUse;
use crate::errors::{SemaError, SemaErrorKind};
use crate::types::{SemaTypeId, Ty};

impl SemaDb {
    pub(super) fn synth_perform(&mut self, e: ExprId, span: Span, expr_id: ExprId) -> SemaTypeId {
        if self.depth == 0 {
            self.errors.push(SemaError {
                kind: SemaErrorKind::PurityViolation {
                    effect: self.db.interner.intern("_"),
                },
                span,
                context: Some("perform outside lambda"),
            });
        }
        if let Some(effect_use) = self.resolve_perform_target(e) {
            let _prev = self.env.perform_effects.insert(expr_id, effect_use);
            if let Some(effect) = self.env.effect_by_id(effect_use.effect_id) {
                let effect_ty = self.env.intern(Ty::Effect(effect.name));
                let _ = self.env.effect_map.insert(expr_id, vec![effect_ty]);
            }
        } else {
            self.errors.push(SemaError {
                kind: SemaErrorKind::NotCallable,
                span,
                context: Some("perform expects an effect operation call"),
            });
        }
        self.synth(e)
    }

    pub(super) fn resolve_perform_target(&mut self, expr_id: ExprId) -> Option<EffectUse> {
        let ExprKind::App(callee, _) = self.db.ast.exprs.get(expr_id).kind.clone() else {
            return None;
        };
        let ExprKind::Access {
            expr,
            field: FieldTarget::Name(op_ident),
            ..
        } = self.db.ast.exprs.get(callee).kind.clone()
        else {
            return None;
        };
        let ExprKind::Var(effect_ident) = self.db.ast.exprs.get(expr).kind.clone() else {
            return None;
        };
        let effect_ty = self.synth_var(expr, &effect_ident);
        let resolved = self.env.resolve_var(effect_ty);
        let effect_name = match self.env.types.get(resolved) {
            Ty::Effect(effect_name) => *effect_name,
            _ => return None,
        };
        if let Some(module_name) = self
            .resolution
            .imported_effect_modules
            .get(&effect_ident.name)
        {
            self.env
                .set_effect_module_name(effect_name, module_name.clone());
        }
        Some(self.env.ensure_effect_op(
            effect_name,
            op_ident.name,
            self.env.builtin(BuiltinType::Unit),
        ))
    }

    pub(super) fn synth_handle(
        &mut self,
        effect: &TyRef,
        clauses: &[HandlerClause],
        body: ExprId,
        span: Span,
        expr_id: ExprId,
    ) -> SemaTypeId {
        let effect_name = effect.name.name;
        if let Some(module_name) = self.resolution.imported_effect_modules.get(&effect_name) {
            self.env
                .set_effect_module_name(effect_name, module_name.clone());
        }
        let effect_id = self.env.assign_effect_id(effect_name);
        let _prev = self.env.handle_effects.insert(expr_id, effect_id);

        let mut saw_return = false;
        let mut seen_ops = HashSet::new();
        let required_ops = self
            .env
            .effect_defs
            .get(&effect_name)
            .map(|effect| effect.operations.clone())
            .unwrap_or_default();

        for clause in clauses {
            match clause {
                HandlerClause::Return { body, .. } => {
                    if saw_return {
                        self.errors.push(SemaError {
                            kind: SemaErrorKind::DuplicateHandler {
                                op: self.db.interner.intern("return"),
                            },
                            span,
                            context: None,
                        });
                    }
                    saw_return = true;
                    let _ = self.synth(*body);
                }
                HandlerClause::Op {
                    name, args, body, ..
                } => {
                    if !seen_ops.insert(name.name) {
                        self.errors.push(SemaError {
                            kind: SemaErrorKind::DuplicateHandler { op: name.name },
                            span,
                            context: None,
                        });
                    }
                    let Some(op_info) = self.env.effect_op_info(effect_name, name.name).cloned()
                    else {
                        self.errors.push(SemaError {
                            kind: SemaErrorKind::UnknownHandlerOp { op: name.name },
                            span,
                            context: None,
                        });
                        let _ = self.synth(*body);
                        continue;
                    };
                    let expected = self.effect_op_arity(op_info.param_ty);
                    if expected != args.len() {
                        self.errors.push(SemaError {
                            kind: SemaErrorKind::ArityMismatch {
                                expected,
                                found: args.len(),
                            },
                            span,
                            context: Some("handler operation payload arity"),
                        });
                    }
                    let prev_handler_ret = self.current_handler_ret;
                    self.current_handler_ret = matches!(
                        self.env.types.get(self.env.resolve_var(op_info.ret_ty)),
                        Ty::Empty
                    )
                    .then_some(op_info.ret_ty);
                    let _ = self.synth(*body);
                    self.current_handler_ret = prev_handler_ret;
                }
            }
        }

        if !saw_return {
            self.errors.push(SemaError {
                kind: SemaErrorKind::MissingReturnHandler,
                span,
                context: None,
            });
        }

        for required_op in &required_ops {
            if !seen_ops.contains(&required_op.name) {
                self.errors.push(SemaError {
                    kind: SemaErrorKind::MissingHandler {
                        op: required_op.name,
                    },
                    span,
                    context: None,
                });
            }
        }

        let prev_handler_ret = self.current_handler_ret;
        self.current_handler_ret = prev_handler_ret;

        self.synth(body)
    }

    pub(super) fn effect_op_arity(&self, param_ty: Option<SemaTypeId>) -> usize {
        let Some(param_ty) = param_ty else {
            return 0;
        };
        let resolved = self.env.resolve_var(param_ty);
        match self.env.types.get(resolved) {
            Ty::Unit => 0,
            Ty::Tuple(elems) => elems.len(),
            _ => 1,
        }
    }

    pub(super) fn synth_resume(&mut self, val: Option<ExprId>, span: Span) -> SemaTypeId {
        if let Some(v) = val {
            let _ = self.synth(v);
        }

        if let Some(ret_ty) = self.current_handler_ret {
            let resolved = self.env.resolve_var(ret_ty);
            if matches!(self.env.types.get(resolved), Ty::Empty) {
                let op_sym = self.db.interner.intern("_");
                self.errors.push(SemaError {
                    kind: SemaErrorKind::ResumeOnNever { op: op_sym },
                    span,
                    context: None,
                });
            }
        }

        self.env.intern(Ty::Empty)
    }
}
