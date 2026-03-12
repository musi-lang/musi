//! Pass 1: top-level definition collection.

use music_ast::ExprIdx;
use music_ast::decl::ForeignDecl;
use music_ast::expr::Expr;

use crate::def::DefKind;

use super::{Resolver, binding_def_kind};

impl Resolver<'_> {
    pub(super) fn collect_top_level(&mut self, expr_idx: ExprIdx) {
        match &self.ast.exprs[expr_idx] {
            Expr::Binding {
                exported, fields, ..
            } => {
                self.define_fn_name(fields.pat, binding_def_kind(fields.kind));
                self.define_pat(fields.pat, binding_def_kind(fields.kind));
                if *exported {
                    self.mark_pat_exported(fields.pat);
                }
            }
            Expr::Class { name, exported, .. } => {
                let id = self
                    .defs
                    .alloc(*name, DefKind::Class, self.span_of_expr(expr_idx));
                if *exported {
                    self.defs.get_mut(id).exported = true;
                }
                self.define_in_scope(*name, id, self.span_of_expr(expr_idx));
            }
            Expr::Effect { name, exported, .. } => {
                let id = self
                    .defs
                    .alloc(*name, DefKind::Effect, self.span_of_expr(expr_idx));
                if *exported {
                    self.defs.get_mut(id).exported = true;
                }
                self.define_in_scope(*name, id, self.span_of_expr(expr_idx));
            }
            Expr::Foreign {
                exported, decls, ..
            } => {
                for decl in decls {
                    match decl {
                        ForeignDecl::Fn { name, span, .. } => {
                            let id = self.defs.alloc(*name, DefKind::ForeignFn, *span);
                            if *exported {
                                self.defs.get_mut(id).exported = true;
                            }
                            self.define_in_scope(*name, id, *span);
                        }
                        ForeignDecl::OpaqueType { name, span } => {
                            let id = self.defs.alloc(*name, DefKind::OpaqueType, *span);
                            if *exported {
                                self.defs.get_mut(id).exported = true;
                            }
                            self.define_in_scope(*name, id, *span);
                        }
                    }
                }
            }
            Expr::Given {
                target, exported, ..
            } => {
                let id = self
                    .defs
                    .alloc(target.name, DefKind::Given, self.span_of_expr(expr_idx));
                if *exported {
                    self.defs.get_mut(id).exported = true;
                }
            }
            Expr::Annotated { inner, .. } => {
                self.collect_top_level(*inner);
            }
            _ => {}
        }
    }
}
