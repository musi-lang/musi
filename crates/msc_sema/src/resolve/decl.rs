//! Declaration resolution: class, given, effect, foreign.

use std::collections::HashSet;

use msc_ast::ExprIdx;
use msc_ast::decl::{ClassMember, EffectOp, ForeignDecl};
use msc_ast::expr::{
    Arg, ArrayElem, Expr, InstanceBody, MatchArm, Param, PwArm, PwGuard, RecField,
};
use msc_ast::ty_param::{Constraint, TyParam};
use msc_ast::util::collect_ty_var_nodes;
use msc_shared::{Span, Symbol};

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
        // Operator members (sentinel Symbol(u32::MAX)) are registered in class_op_members
        // instead of the name scope - they're resolved via operator dispatch.
        for (member_name, def_id) in member_defs {
            if member_name == Symbol(u32::MAX) {
                if let Some(class_def) = class_def {
                    let _prev = self
                        .output
                        .class_op_members
                        .insert((class_def, member_name), def_id);
                }
                continue;
            }
            // Register non-sentinel members in class_op_members too, keyed by their symbol,
            // so the checker can find operator methods that have real interned names.
            if let Some(class_def) = class_def {
                let _prev = self
                    .output
                    .class_op_members
                    .insert((class_def, member_name), def_id);
            }
            let span = self.defs.get(def_id).span;
            self.define_in_scope(member_name, def_id, span);
        }
    }

    pub(super) fn resolve_expr_given(
        &mut self,
        target: ExprIdx,
        params: &[TyParam],
        constraints: &[Constraint],
        members: &[ClassMember],
    ) {
        let mut all_params: Vec<TyParam> = params.to_vec();
        collect_ty_var_nodes(target, self.ast, &mut all_params);
        all_params.retain(|p| self.scopes.lookup(self.current_scope, p.name).is_none());
        let parent = self.enter_ty_param_scope(&all_params, constraints);
        self.resolve_type_expr(target);
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
            let op_id = self
                .defs
                .alloc(op.name, DefKind::EffectOp, op.span, self.file_id);
            if let Some(eff) = effect_def {
                self.defs.get_mut(op_id).parent = Some(eff);
            }
            if exported {
                self.defs.get_mut(op_id).exported = true;
            }
            self.resolve_type_expr(op.ty);
        }
        self.current_scope = parent;
    }

    pub(super) fn resolve_expr_foreign(&mut self, decls: &[ForeignDecl]) {
        for decl in decls {
            if let ForeignDecl::Fn { ty, .. } = decl {
                let mut ty_params = vec![];
                collect_ty_var_nodes(*ty, self.ast, &mut ty_params);
                ty_params.retain(|p| self.scopes.lookup(self.current_scope, p.name).is_none());
                let parent = if ty_params.is_empty() {
                    None
                } else {
                    Some(self.enter_ty_param_scope(&ty_params, &[]))
                };
                self.resolve_type_expr(*ty);
                if let Some(p) = parent {
                    self.current_scope = p;
                }
            }
        }
    }

    fn resolve_class_members(
        &mut self,
        members: &[ClassMember],
        parent_def: Option<DefId>,
    ) -> Vec<(Symbol, DefId)> {
        let mut member_defs = vec![];

        // Pass 1: allocate all member DefIds and define them in the current scope
        // so sibling methods can reference each other.
        for member in members {
            if let ClassMember::Fn { sig, .. } = member {
                let fn_id = self
                    .defs
                    .alloc(sig.name, DefKind::Fn, sig.span, self.file_id);
                self.defs.get_mut(fn_id).exported = true;
                if let Some(pd) = parent_def {
                    self.defs.get_mut(fn_id).parent = Some(pd);
                }
                self.define_in_scope(sig.name, fn_id, sig.span);
                let _inserted = self.output.pat_defs.insert(sig.span, fn_id);
                member_defs.push((sig.name, fn_id));
            }
            if let ClassMember::Law { name, span, .. } = member {
                let law_id = self.defs.alloc(*name, DefKind::Law, *span, self.file_id);
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
                            let id = self.defs.alloc(
                                param.name,
                                DefKind::Param,
                                param.span,
                                self.file_id,
                            );
                            self.defs.get_mut(id).param_mode = Some(param.mode);
                            self.define_in_scope(param.name, id, param.span);
                            let _inserted = self.output.pat_defs.insert(param.span, id);
                            if let Some(ty) = param.ty {
                                self.resolve_type_expr(ty);
                            }
                        }
                        if let Some(ret) = sig.ret {
                            self.resolve_type_expr(ret);
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
                let id = self.defs.alloc(*sym, DefKind::LawVar, *span, self.file_id);
                self.define_in_scope(*sym, id, *span);
                free_defs.push((*sym, id));
            }
            if !free_defs.is_empty() {
                let _prev = self.output.law_inferred_vars.insert(law_span, free_defs);
            }
        } else {
            for param in params {
                let id = self
                    .defs
                    .alloc(param.name, DefKind::LawVar, param.span, self.file_id);
                self.define_in_scope(param.name, id, param.span);
                let _inserted = self.output.pat_defs.insert(param.span, id);
                if let Some(ty) = param.ty {
                    self.resolve_type_expr(ty);
                }
            }
        }

        self.resolve_expr(body);
        self.current_scope = parent;
    }

    fn collect_free_names(&self, expr_idx: ExprIdx) -> Vec<(Symbol, Span)> {
        let mut free = vec![];
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
            Expr::Name { name_ref, span } => {
                let name = self.ast.name_refs[*name_ref].name;
                if self.scopes.lookup(self.current_scope, name).is_none() && seen.insert(name) {
                    free.push((name, *span));
                }
            }
            Expr::BinOp { left, right, .. }
            | Expr::Index {
                object: left,
                index: right,
                ..
            } => {
                self.collect_free_names_inner(*left, free, seen);
                self.collect_free_names_inner(*right, free, seen);
            }
            Expr::UnaryOp { operand, .. }
            | Expr::Paren { inner: operand, .. }
            | Expr::Field {
                object: operand, ..
            }
            | Expr::TypeCheck { operand, .. }
            | Expr::Annotated { inner: operand, .. }
            | Expr::Fn { body: operand, .. } => {
                self.collect_free_names_inner(*operand, free, seen);
            }
            Expr::Call { callee, args, .. } => {
                self.collect_free_names_inner(*callee, free, seen);
                for arg in args {
                    if let Arg::Pos { expr, .. } = arg {
                        self.collect_free_names_inner(*expr, free, seen);
                    }
                }
            }
            Expr::Tuple { elems, .. } | Expr::Variant { args: elems, .. } => {
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
            Expr::Return { value, .. } => {
                if let Some(v) = value {
                    self.collect_free_names_inner(*v, free, seen);
                }
            }
            expr => self.collect_free_names_compound(expr, free, seen),
        }
    }

    fn collect_free_names_compound(
        &self,
        expr: &Expr,
        free: &mut Vec<(Symbol, Span)>,
        seen: &mut HashSet<Symbol>,
    ) {
        match expr {
            Expr::Let { fields, .. } => {
                if let Some(v) = fields.value {
                    self.collect_free_names_inner(v, free, seen);
                }
            }
            Expr::Update { base, fields, .. } => {
                self.collect_free_names_inner(*base, free, seen);
                self.collect_free_rec_fields(fields, free, seen);
            }
            Expr::Record { fields, .. } => {
                self.collect_free_rec_fields(fields, free, seen);
            }
            Expr::Array { elems, .. } => {
                self.collect_free_array_elems(elems, free, seen);
            }
            Expr::Piecewise { arms, .. } => {
                self.collect_free_pw_arms(arms, free, seen);
            }
            Expr::Match {
                scrutinee, arms, ..
            } => {
                self.collect_free_match_arms(*scrutinee, arms, free, seen);
            }
            Expr::Class { members, .. }
            | Expr::Instance {
                body: InstanceBody::Manual { members },
                ..
            } => {
                self.collect_free_class_members(members, free, seen);
            }
            Expr::Handle { body, ops, .. } => {
                self.collect_free_names_inner(*body, free, seen);
                for op in ops {
                    self.collect_free_names_inner(op.body, free, seen);
                }
            }
            _ => {}
        }
    }

    fn collect_free_array_elems(
        &self,
        elems: &[ArrayElem],
        free: &mut Vec<(Symbol, Span)>,
        seen: &mut HashSet<Symbol>,
    ) {
        for elem in elems {
            let e = match elem {
                ArrayElem::Elem { expr, .. } | ArrayElem::Spread { expr, .. } => *expr,
            };
            self.collect_free_names_inner(e, free, seen);
        }
    }

    fn collect_free_pw_arms(
        &self,
        arms: &[PwArm],
        free: &mut Vec<(Symbol, Span)>,
        seen: &mut HashSet<Symbol>,
    ) {
        for arm in arms {
            if let PwGuard::When { expr, .. } = arm.guard {
                self.collect_free_names_inner(expr, free, seen);
            }
            self.collect_free_names_inner(arm.result, free, seen);
        }
    }

    fn collect_free_match_arms(
        &self,
        scrutinee: ExprIdx,
        arms: &[MatchArm],
        free: &mut Vec<(Symbol, Span)>,
        seen: &mut HashSet<Symbol>,
    ) {
        self.collect_free_names_inner(scrutinee, free, seen);
        for arm in arms {
            if let Some(guard) = arm.guard {
                self.collect_free_names_inner(guard, free, seen);
            }
            self.collect_free_names_inner(arm.result, free, seen);
        }
    }

    fn collect_free_rec_fields(
        &self,
        fields: &[RecField],
        free: &mut Vec<(Symbol, Span)>,
        seen: &mut HashSet<Symbol>,
    ) {
        for field in fields {
            match field {
                RecField::Named { value: Some(v), .. } => {
                    self.collect_free_names_inner(*v, free, seen);
                }
                RecField::Named { value: None, .. } => {}
                RecField::Spread { expr, .. } => {
                    self.collect_free_names_inner(*expr, free, seen);
                }
            }
        }
    }

    fn collect_free_class_members(
        &self,
        members: &[ClassMember],
        free: &mut Vec<(Symbol, Span)>,
        seen: &mut HashSet<Symbol>,
    ) {
        for member in members {
            match member {
                ClassMember::Fn {
                    default: Some(body),
                    ..
                }
                | ClassMember::Law { body, .. } => {
                    self.collect_free_names_inner(*body, free, seen);
                }
                ClassMember::Fn { default: None, .. } => {}
            }
        }
    }
}
