//! Declaration resolution: class, given, effect, foreign.

use std::collections::HashSet;

use music_ast::ExprIdx;
use music_ast::decl::{ClassMember, EffectOp, ForeignDecl};
use music_ast::expr::{Arg, Expr, Param};
use music_ast::ty::{Constraint, TyNamedRef, TyParam};
use music_ast::util::collect_ty_var_nodes;
use music_shared::{Span, Symbol};

use crate::def::{DefId, DefKind};

use super::Resolver;

impl Resolver<'_> {
    pub(super) fn resolve_expr_class(
        &mut self,
        name: Symbol,
        params: &[TyParam],
        constraints: &[Constraint],
        members: &[ClassMember],
    ) {
        let outer = self.current_scope;
        let parent = self.enter_ty_param_scope(params, constraints);
        let class_def = self.scopes.lookup(outer, name);
        let member_defs = self.resolve_class_members(members, class_def);
        self.current_scope = parent;

        // Export class members to the enclosing scope (Haskell-style).
        // Skip operator identifiers (sentinel Symbol) — they're resolved via dispatch.
        for (member_name, def_id) in member_defs {
            if member_name == Symbol(u32::MAX) {
                continue;
            }
            let span = self.defs.get(def_id).span;
            self.define_in_scope(member_name, def_id, span);
        }
    }

    pub(super) fn resolve_expr_given(
        &mut self,
        target: &TyNamedRef,
        params: &[TyParam],
        constraints: &[Constraint],
        members: &[ClassMember],
    ) {
        let mut all_params: Vec<TyParam> = params.to_vec();
        for &arg in &target.args {
            collect_ty_var_nodes(arg, self.ast, &mut all_params);
        }
        let parent = self.enter_ty_param_scope(&all_params, constraints);
        self.resolve_ty_named_ref(target);
        let _member_defs = self.resolve_class_members(members, None);
        self.current_scope = parent;
    }

    pub(super) fn resolve_expr_effect(
        &mut self,
        name: Symbol,
        params: &[TyParam],
        ops: &[EffectOp],
        exported: bool,
    ) {
        let outer = self.current_scope;
        let parent = self.enter_ty_param_scope(params, &[]);
        let effect_def = self.scopes.lookup(outer, name);
        for op in ops {
            let op_id = self.defs.alloc(op.name, DefKind::EffectOp, op.span);
            if let Some(eff) = effect_def {
                self.defs.get_mut(op_id).parent = Some(eff);
            }
            if exported {
                self.defs.get_mut(op_id).exported = true;
            }
            self.resolve_ty(op.ty);
        }
        self.current_scope = parent;
    }

    pub(super) fn resolve_expr_foreign(&mut self, decls: &[ForeignDecl]) {
        for decl in decls {
            if let ForeignDecl::Fn { ty, .. } = decl {
                self.resolve_ty(*ty);
            }
        }
    }

    fn resolve_class_members(
        &mut self,
        members: &[ClassMember],
        parent_def: Option<DefId>,
    ) -> Vec<(Symbol, DefId)> {
        let mut member_defs = Vec::new();

        // Pass 1: allocate all member DefIds and define them in the current scope
        // so sibling methods can reference each other.
        for member in members {
            if let ClassMember::Fn { sig, .. } = member {
                let fn_id = self.defs.alloc(sig.name, DefKind::Fn, sig.span);
                self.defs.get_mut(fn_id).exported = true;
                if let Some(pd) = parent_def {
                    self.defs.get_mut(fn_id).parent = Some(pd);
                }
                self.define_in_scope(sig.name, fn_id, sig.span);
                let _inserted = self.output.pat_defs.insert(sig.span, fn_id);
                member_defs.push((sig.name, fn_id));
            }
            if let ClassMember::Law { name, span, .. } = member {
                let law_id = self.defs.alloc(*name, DefKind::Law, *span);
                if let Some(pd) = parent_def {
                    self.defs.get_mut(law_id).parent = Some(pd);
                }
                let _inserted = self.output.pat_defs.insert(*span, law_id);
            }
        }

        // Pass 2: resolve member bodies.
        for member in members {
            match member {
                ClassMember::Fn { sig, default, .. } => {
                    if let Some(body) = default {
                        let parent = self.current_scope;
                        self.current_scope = self.scopes.push_child(parent);
                        for param in &sig.params {
                            let id = self.defs.alloc(param.name, DefKind::Param, param.span);
                            self.defs.get_mut(id).param_mode = Some(param.mode);
                            self.define_in_scope(param.name, id, param.span);
                            let _inserted = self.output.pat_defs.insert(param.span, id);
                            if let Some(ty) = param.ty {
                                self.resolve_ty(ty);
                            }
                        }
                        if let Some(ret) = sig.ret {
                            self.resolve_ty(ret);
                        }
                        self.resolve_expr(*body);
                        self.current_scope = parent;
                    }
                }
                ClassMember::Law {
                    params, body, span, ..
                } => {
                    self.resolve_law_body(params, *body, *span);
                }
            }
        }

        member_defs
    }

    fn resolve_law_body(&mut self, params: &[Param], body: ExprIdx, law_span: Span) {
        let parent = self.current_scope;
        self.current_scope = self.scopes.push_child(parent);

        if params.is_empty() {
            let free = self.collect_free_names(body);
            let mut free_defs = Vec::with_capacity(free.len());
            for (sym, span) in &free {
                let id = self.defs.alloc(*sym, DefKind::LawVar, *span);
                self.define_in_scope(*sym, id, *span);
                free_defs.push((*sym, id));
            }
            if !free_defs.is_empty() {
                let _prev = self.output.law_inferred_vars.insert(law_span, free_defs);
            }
        } else {
            for param in params {
                let id = self.defs.alloc(param.name, DefKind::LawVar, param.span);
                self.define_in_scope(param.name, id, param.span);
                let _inserted = self.output.pat_defs.insert(param.span, id);
                if let Some(ty) = param.ty {
                    self.resolve_ty(ty);
                }
            }
        }

        self.resolve_expr(body);
        self.current_scope = parent;
    }

    fn collect_free_names(&self, expr_idx: ExprIdx) -> Vec<(Symbol, Span)> {
        let mut free = Vec::new();
        let mut seen = HashSet::new();
        self.collect_free_names_inner(expr_idx, &mut free, &mut seen);
        free
    }

    fn collect_free_names_inner(
        &self,
        expr_idx: ExprIdx,
        free: &mut Vec<(Symbol, Span)>,
        seen: &mut HashSet<Symbol>,
    ) {
        match &self.ast.exprs[expr_idx] {
            Expr::Name { name, span } => {
                if self.scopes.lookup(self.current_scope, *name).is_none() && seen.insert(*name) {
                    free.push((*name, *span));
                }
            }
            Expr::BinOp { left, right, .. } => {
                self.collect_free_names_inner(*left, free, seen);
                self.collect_free_names_inner(*right, free, seen);
            }
            Expr::UnaryOp { operand, .. } => {
                self.collect_free_names_inner(*operand, free, seen);
            }
            Expr::Paren { inner, .. } => {
                self.collect_free_names_inner(*inner, free, seen);
            }
            Expr::Call { callee, args, .. } => {
                self.collect_free_names_inner(*callee, free, seen);
                for arg in args {
                    if let Arg::Pos { expr, .. } = arg {
                        self.collect_free_names_inner(*expr, free, seen);
                    }
                }
            }
            Expr::Tuple { elems, .. } => {
                for &e in elems {
                    self.collect_free_names_inner(e, free, seen);
                }
            }
            Expr::Block { stmts, tail, .. } => {
                for &s in stmts {
                    self.collect_free_names_inner(s, free, seen);
                }
                if let Some(t) = tail {
                    self.collect_free_names_inner(*t, free, seen);
                }
            }
            _ => {}
        }
    }
}
