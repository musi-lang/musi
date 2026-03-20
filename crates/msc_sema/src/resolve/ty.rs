//! Type expression resolution helpers.

use msc_ast::ExprIdx;
use msc_ast::expr::{EffectItem, Expr, TypeForm};

use super::Resolver;

impl Resolver<'_> {
    pub(super) fn resolve_type_expr(&mut self, expr_idx: ExprIdx) {
        match self.ast.exprs[expr_idx].clone() {
            Expr::Name { name_ref, span } => {
                let nr = self.ast.name_refs[name_ref];
                if let Some(def_id) = self.scopes.lookup(self.current_scope, nr.name) {
                    self.output.name_ref_defs[usize::try_from(name_ref.raw()).unwrap()] =
                        Some(def_id);
                    self.defs.get_mut(def_id).use_count += 1;
                } else {
                    self.report_undefined(nr.name, span);
                }
            }
            Expr::TypeApp { callee, args, .. } => {
                self.resolve_type_expr(callee);
                for &arg in &args {
                    self.resolve_type_expr(arg);
                }
            }
            Expr::Field { object, .. } => {
                // Qualified type: M.Type - resolve the module name.
                // The field name itself is not resolved here (handled by checker).
                self.resolve_type_expr(object);
            }
            Expr::TypeExpr { kind, .. } => match kind {
                TypeForm::Option { inner } | TypeForm::Array { elem: inner, .. } => {
                    self.resolve_type_expr(inner);
                }
                TypeForm::Product { fields, .. } | TypeForm::Sum { variants: fields } => {
                    for f in fields {
                        self.resolve_type_expr(f);
                    }
                }
                TypeForm::Pi { param_ty, body, .. } => {
                    self.resolve_type_expr(param_ty);
                    self.resolve_type_expr(body);
                }
            },
            Expr::FnType {
                params,
                ret,
                effects,
                ..
            } => {
                for &p in &params {
                    self.resolve_type_expr(p);
                }
                self.resolve_type_expr(ret);
                if let Some(eff) = &effects {
                    for item in &eff.effects {
                        match item {
                            EffectItem::Named { arg, .. } => {
                                if let Some(a) = arg {
                                    self.resolve_type_expr(*a);
                                }
                            }
                            EffectItem::Var { .. } => {}
                        }
                    }
                }
            }
            _ => {}
        }
    }
}
