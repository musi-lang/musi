//! Two-pass name resolution.
//!
//! Pass 1: collect all top-level definitions (bindings, functions, types,
//! classes, effects) into the module scope.
//!
//! Pass 2: walk the full AST, resolving every name reference to a [`DefId`]
//! and creating child scopes for blocks, functions, and match arms.

#[cfg(test)]
mod tests;

use std::collections::HashMap;

use music_ast::decl::{ClassMember, EffectOp};
use music_ast::expr::{
    Arg, ArrayElem, BindKind, Expr, LetFields, MatchArm, Param, PwGuard, RecField,
};
use music_ast::pat::Pat;
use music_ast::ty::{Constraint, Ty, TyNamedRef, TyParam};
use music_ast::{AstArenas, ParsedModule};
use music_shared::{DiagnosticBag, FileId, Idx, Interner, Span, Symbol};

use crate::def::{DefId, DefKind, DefTable};
use crate::error::SemaError;
use crate::scope::{ScopeId, ScopeTree};

/// Output accumulators from the resolution pass.
pub struct ResolveOutput {
    pub expr_defs: HashMap<Idx<Expr>, DefId>,
    pub pat_defs: HashMap<Span, DefId>,
}

/// Runs two-pass name resolution over a parsed module.
#[must_use]
pub fn resolve(
    module: &ParsedModule,
    interner: &mut Interner,
    file_id: FileId,
    diags: &mut DiagnosticBag,
    defs: &mut DefTable,
    scopes: &mut ScopeTree,
    module_scope: ScopeId,
) -> ResolveOutput {
    let mut resolver = Resolver {
        ast: &module.arenas,
        interner,
        file_id,
        diags,
        defs,
        scopes,
        output: ResolveOutput {
            expr_defs: HashMap::new(),
            pat_defs: HashMap::new(),
        },
        current_scope: module_scope,
    };

    for stmt in &module.stmts {
        resolver.collect_top_level(stmt.expr);
    }
    for stmt in &module.stmts {
        resolver.resolve_expr(stmt.expr);
    }

    resolver.output
}

struct Resolver<'a> {
    ast: &'a AstArenas,
    interner: &'a mut Interner,
    file_id: FileId,
    diags: &'a mut DiagnosticBag,
    defs: &'a mut DefTable,
    scopes: &'a mut ScopeTree,
    output: ResolveOutput,
    current_scope: ScopeId,
}

impl Resolver<'_> {
    fn collect_top_level(&mut self, expr_idx: Idx<Expr>) {
        match &self.ast.exprs[expr_idx] {
            Expr::Binding { fields, .. } => {
                self.define_pat(fields.pat, binding_def_kind(fields.kind));
            }
            Expr::Class { name, .. } => {
                let id = self
                    .defs
                    .alloc(*name, DefKind::Class, self.span_of_expr(expr_idx));
                self.define_in_scope(*name, id, self.span_of_expr(expr_idx));
            }
            Expr::Effect { name, .. } => {
                let id = self
                    .defs
                    .alloc(*name, DefKind::Effect, self.span_of_expr(expr_idx));
                self.define_in_scope(*name, id, self.span_of_expr(expr_idx));
            }
            Expr::Annotated { inner, .. } => {
                self.collect_top_level(*inner);
            }
            _ => {}
        }
    }

    /// Pass 2: resolve all name references in an expression.
    fn resolve_expr(&mut self, expr_idx: Idx<Expr>) {
        match self.ast.exprs[expr_idx].clone() {
            Expr::Name { name, span } => self.resolve_name(expr_idx, name, span),
            Expr::Lit { .. } | Expr::Error { .. } | Expr::Import { .. } | Expr::Export { .. } => {}
            Expr::Paren { inner, .. } | Expr::Annotated { inner, .. } => self.resolve_expr(inner),
            Expr::Tuple { elems, .. } | Expr::Variant { args: elems, .. } => {
                for &e in &elems {
                    self.resolve_expr(e);
                }
            }
            Expr::Block { stmts, tail, .. } => self.resolve_expr_block(&stmts, tail),
            Expr::BinOp { left, right, .. } => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            }
            Expr::UnaryOp { operand, .. } => self.resolve_expr(operand),
            Expr::Field { object, .. } => self.resolve_expr(object),
            Expr::Index { object, index, .. } => {
                self.resolve_expr(object);
                self.resolve_expr(index);
            }
            Expr::Call { callee, args, .. } => self.resolve_expr_call(callee, &args),
            Expr::Update { base, fields, .. } => {
                self.resolve_expr(base);
                self.resolve_rec_fields(&fields);
            }
            Expr::Record { fields, .. } => self.resolve_rec_fields(&fields),
            Expr::Array { elems, .. } => {
                for elem in &elems {
                    match elem {
                        ArrayElem::Elem { expr, .. } | ArrayElem::Spread { expr, .. } => {
                            self.resolve_expr(*expr);
                        }
                    }
                }
            }
            Expr::Piecewise { arms, .. } => {
                for arm in &arms {
                    if let PwGuard::When { expr, .. } = arm.guard {
                        self.resolve_expr(expr);
                    }
                    self.resolve_expr(arm.result);
                }
            }
            Expr::Return { value, .. } => {
                if let Some(v) = value {
                    self.resolve_expr(v);
                }
            }
            Expr::Let { fields, body, .. } => self.resolve_expr_let(&fields, body),
            Expr::Binding { fields, .. } => self.resolve_expr_binding(&fields),
            Expr::Fn {
                params,
                ret_ty,
                body,
                ..
            } => self.resolve_expr_fn(&params, ret_ty, body),
            Expr::Match {
                scrutinee, arms, ..
            } => self.resolve_expr_match(scrutinee, &arms),
            Expr::Quantified {
                body,
                params,
                constraints,
                ..
            } => {
                self.resolve_expr_quantified(body, &params, &constraints);
            }
            Expr::Class {
                name,
                params,
                constraints,
                members,
                ..
            } => {
                self.resolve_expr_class(name, &params, &constraints, &members);
            }
            Expr::Given {
                target,
                params,
                constraints,
                members,
                ..
            } => {
                self.resolve_expr_given(&target, &params, &constraints, &members);
            }
            Expr::Effect {
                name, params, ops, ..
            } => self.resolve_expr_effect(name, &params, &ops),
        }
    }

    fn resolve_name(&mut self, expr_idx: Idx<Expr>, name: Symbol, span: Span) {
        if let Some(def_id) = self.scopes.lookup(self.current_scope, name) {
            let _prev = self.output.expr_defs.insert(expr_idx, def_id);
            self.defs.get_mut(def_id).use_count += 1;
        } else {
            self.report_undefined(name, span);
        }
    }

    fn report_undefined(&mut self, name: Symbol, span: Span) {
        let name_str = self.interner.resolve(name);
        let _d = self.diags.report(
            &SemaError::UndefinedName {
                name: Box::from(name_str),
            },
            span,
            self.file_id,
        );
    }

    fn resolve_rec_fields(&mut self, fields: &[RecField]) {
        for field in fields {
            match field {
                RecField::Named { value, .. } => {
                    if let Some(v) = value {
                        self.resolve_expr(*v);
                    }
                }
                RecField::Spread { expr, .. } => {
                    self.resolve_expr(*expr);
                }
            }
        }
    }

    fn enter_ty_param_scope(&mut self, params: &[TyParam], constraints: &[Constraint]) -> ScopeId {
        let parent = self.current_scope;
        self.current_scope = self.scopes.push_child(parent);
        for param in params {
            let id = self.defs.alloc(param.name, DefKind::Type, param.span);
            self.define_in_scope(param.name, id, param.span);
        }
        for constraint in constraints {
            self.resolve_ty_named_ref(&constraint.bound);
        }
        parent
    }

    fn resolve_class_members(&mut self, members: &[ClassMember], parent_def: Option<DefId>) {
        for member in members {
            match member {
                ClassMember::Fn { sig, default, .. } => {
                    let fn_id = self.defs.alloc(sig.name, DefKind::Fn, sig.span);
                    if let Some(pd) = parent_def {
                        self.defs.get_mut(fn_id).parent = Some(pd);
                    }
                    if let Some(body) = default {
                        self.resolve_expr(*body);
                    }
                }
                ClassMember::Law { body, .. } => {
                    self.resolve_expr(*body);
                }
            }
        }
    }

    fn resolve_expr_block(&mut self, stmts: &[Idx<Expr>], tail: Option<Idx<Expr>>) {
        let parent = self.current_scope;
        self.current_scope = self.scopes.push_child(parent);
        for &stmt in stmts {
            // Handle block-local bindings: resolve value/type, then define in block scope.
            if let Expr::Binding { fields, .. } = &self.ast.exprs[stmt] {
                self.resolve_expr(fields.value);
                if let Some(ty) = fields.ty {
                    self.resolve_ty(ty);
                }
                self.define_pat(fields.pat, binding_def_kind(fields.kind));
            } else {
                self.resolve_expr(stmt);
            }
        }
        if let Some(t) = tail {
            self.resolve_expr(t);
        }
        self.current_scope = parent;
    }

    fn resolve_expr_call(&mut self, callee: Idx<Expr>, args: &[Arg]) {
        self.resolve_expr(callee);
        for arg in args {
            if let Arg::Pos { expr, .. } = arg {
                self.resolve_expr(*expr);
            }
        }
    }

    fn resolve_expr_let(&mut self, fields: &LetFields, body: Option<Idx<Expr>>) {
        self.resolve_expr(fields.value);
        if let Some(ty) = fields.ty {
            self.resolve_ty(ty);
        }

        if let Some(body) = body {
            let parent = self.current_scope;
            self.current_scope = self.scopes.push_child(parent);
            self.define_pat(fields.pat, binding_def_kind(fields.kind));
            self.resolve_expr(body);
            self.current_scope = parent;
        } else {
            self.define_pat(fields.pat, binding_def_kind(fields.kind));
        }
    }

    fn resolve_expr_binding(&mut self, fields: &LetFields) {
        self.resolve_expr(fields.value);
        if let Some(ty) = fields.ty {
            self.resolve_ty(ty);
        }
    }

    fn resolve_expr_fn(&mut self, params: &[Param], ret_ty: Option<Idx<Ty>>, body: Idx<Expr>) {
        let parent = self.current_scope;
        self.current_scope = self.scopes.push_child(parent);
        for param in params {
            let id = self.defs.alloc(param.name, DefKind::Param, param.span);
            self.define_in_scope(param.name, id, param.span);
            let _inserted = self.output.pat_defs.insert(param.span, id);
            if let Some(ty) = param.ty {
                self.resolve_ty(ty);
            }
        }
        if let Some(ret) = ret_ty {
            self.resolve_ty(ret);
        }
        self.resolve_expr(body);
        self.current_scope = parent;
    }

    fn resolve_expr_match(&mut self, scrutinee: Idx<Expr>, arms: &[MatchArm]) {
        self.resolve_expr(scrutinee);
        for arm in arms {
            let parent = self.current_scope;
            self.current_scope = self.scopes.push_child(parent);
            self.resolve_pat(arm.pat);
            if let Some(guard) = arm.guard {
                self.resolve_expr(guard);
            }
            self.resolve_expr(arm.result);
            self.current_scope = parent;
        }
    }

    fn resolve_expr_quantified(
        &mut self,
        body: Idx<Expr>,
        params: &[TyParam],
        constraints: &[Constraint],
    ) {
        let parent = self.enter_ty_param_scope(params, constraints);
        self.resolve_expr(body);
        self.current_scope = parent;
    }

    fn resolve_expr_class(
        &mut self,
        name: Symbol,
        params: &[TyParam],
        constraints: &[Constraint],
        members: &[ClassMember],
    ) {
        let outer = self.current_scope;
        let parent = self.enter_ty_param_scope(params, constraints);
        let class_def = self.scopes.lookup(outer, name);
        self.resolve_class_members(members, class_def);
        self.current_scope = parent;
    }

    fn resolve_expr_given(
        &mut self,
        target: &TyNamedRef,
        params: &[TyParam],
        constraints: &[Constraint],
        members: &[ClassMember],
    ) {
        self.resolve_ty_named_ref(target);
        let parent = self.enter_ty_param_scope(params, constraints);
        self.resolve_class_members(members, None);
        self.current_scope = parent;
    }

    fn resolve_expr_effect(&mut self, name: Symbol, params: &[TyParam], ops: &[EffectOp]) {
        let outer = self.current_scope;
        let parent = self.enter_ty_param_scope(params, &[]);
        let effect_def = self.scopes.lookup(outer, name);
        for op in ops {
            let op_id = self.defs.alloc(op.name, DefKind::EffectOp, op.span);
            if let Some(eff) = effect_def {
                self.defs.get_mut(op_id).parent = Some(eff);
            }
            self.resolve_ty(op.ty);
        }
        self.current_scope = parent;
    }

    /// Resolve names in a type annotation.
    fn resolve_ty(&mut self, ty_idx: Idx<Ty>) {
        match self.ast.tys[ty_idx].clone() {
            Ty::Named { name, args, span } => {
                if self.scopes.lookup(self.current_scope, name).is_none() {
                    self.report_undefined(name, span);
                }
                for &arg in &args {
                    self.resolve_ty(arg);
                }
            }
            Ty::Option { inner, .. } | Ty::Ref { inner, .. } => {
                self.resolve_ty(inner);
            }
            Ty::Fn { params, ret, .. } => {
                for &p in &params {
                    self.resolve_ty(p);
                }
                self.resolve_ty(ret);
            }
            Ty::Product { fields, .. } => {
                for &f in &fields {
                    self.resolve_ty(f);
                }
            }
            Ty::Sum { variants, .. } => {
                for &v in &variants {
                    self.resolve_ty(v);
                }
            }
            Ty::Record { fields, .. } => {
                for f in &fields {
                    self.resolve_ty(f.ty);
                }
            }
            Ty::Refine { base, pred, .. } => {
                self.resolve_ty(base);
                self.resolve_expr(pred);
            }
            Ty::Array { elem, .. } => {
                self.resolve_ty(elem);
            }
            Ty::Quantified {
                params,
                constraints,
                body,
                ..
            } => {
                let parent = self.enter_ty_param_scope(&params, &constraints);
                self.resolve_ty(body);
                self.current_scope = parent;
            }
            Ty::Var { .. } | Ty::Error { .. } => {}
        }
    }

    fn resolve_ty_named_ref(&mut self, named: &TyNamedRef) {
        if self.scopes.lookup(self.current_scope, named.name).is_none() {
            self.report_undefined(named.name, named.span);
        }
        for &arg in &named.args {
            self.resolve_ty(arg);
        }
    }

    /// Resolve pattern bindings, creating defs for each bound name.
    fn resolve_pat(&mut self, pat_idx: Idx<Pat>) {
        match self.ast.pats[pat_idx].clone() {
            Pat::Bind {
                name, span, inner, ..
            } => {
                let id = self.defs.alloc(name, DefKind::Let, span);
                self.define_in_scope(name, id, span);
                let _inserted = self.output.pat_defs.insert(span, id);
                if let Some(inner) = inner {
                    self.resolve_pat(inner);
                }
            }
            Pat::Variant { args, .. } => {
                for &arg in &args {
                    self.resolve_pat(arg);
                }
            }
            Pat::Record { fields, .. } => {
                for field in &fields {
                    if let Some(pat) = field.pat {
                        self.resolve_pat(pat);
                    }
                }
            }
            Pat::Tuple { elems, .. } | Pat::Array { elems, .. } => {
                for &elem in &elems {
                    self.resolve_pat(elem);
                }
            }
            Pat::Or { left, right, .. } => {
                self.resolve_pat(left);
                self.resolve_pat(right);
            }
            Pat::Wild { .. } | Pat::Lit { .. } | Pat::Error { .. } => {}
        }
    }

    /// Define a pattern's bindings in the current scope.
    fn define_pat(&mut self, pat_idx: Idx<Pat>, kind: DefKind) {
        match self.ast.pats[pat_idx].clone() {
            Pat::Bind {
                name, span, inner, ..
            } => {
                let id = self.defs.alloc(name, kind, span);
                self.define_in_scope(name, id, span);
                let _inserted = self.output.pat_defs.insert(span, id);
                if let Some(inner) = inner {
                    self.define_pat(inner, kind);
                }
            }
            Pat::Variant { args, .. } => {
                for &arg in &args {
                    self.define_pat(arg, kind);
                }
            }
            Pat::Record { fields, .. } => {
                for field in &fields {
                    if let Some(pat) = field.pat {
                        self.define_pat(pat, kind);
                    }
                }
            }
            Pat::Tuple { elems, .. } | Pat::Array { elems, .. } => {
                for &elem in &elems {
                    self.define_pat(elem, kind);
                }
            }
            Pat::Or { left, right, .. } => {
                self.define_pat(left, kind);
                self.define_pat(right, kind);
            }
            Pat::Wild { .. } | Pat::Lit { .. } | Pat::Error { .. } => {}
        }
    }

    fn define_in_scope(&mut self, name: Symbol, def_id: DefId, span: Span) {
        if let Some(prev) = self.scopes.define(self.current_scope, name, def_id) {
            let name_str = self.interner.resolve(name);
            let prev_span = self.defs.get(prev).span;
            let d = self.diags.report(
                &SemaError::DuplicateDefinition {
                    name: Box::from(name_str),
                },
                span,
                self.file_id,
            );
            if prev_span != Span::DUMMY {
                let _s = d.add_secondary(prev_span, self.file_id, "previous definition here");
            }
        }
    }

    fn span_of_expr(&self, idx: Idx<Expr>) -> Span {
        expr_span(&self.ast.exprs[idx])
    }
}

const fn binding_def_kind(kind: BindKind) -> DefKind {
    match kind {
        BindKind::Immut => DefKind::Let,
        BindKind::Mut => DefKind::Var,
    }
}

pub(crate) const fn expr_span(expr: &Expr) -> Span {
    match expr {
        Expr::Lit { span, .. }
        | Expr::Name { span, .. }
        | Expr::Paren { span, .. }
        | Expr::Tuple { span, .. }
        | Expr::Block { span, .. }
        | Expr::Let { span, .. }
        | Expr::Fn { span, .. }
        | Expr::Call { span, .. }
        | Expr::Field { span, .. }
        | Expr::Index { span, .. }
        | Expr::Update { span, .. }
        | Expr::Record { span, .. }
        | Expr::Array { span, .. }
        | Expr::Variant { span, .. }
        | Expr::BinOp { span, .. }
        | Expr::UnaryOp { span, .. }
        | Expr::Piecewise { span, .. }
        | Expr::Match { span, .. }
        | Expr::Return { span, .. }
        | Expr::Quantified { span, .. }
        | Expr::Import { span, .. }
        | Expr::Export { span, .. }
        | Expr::Annotated { span, .. }
        | Expr::Binding { span, .. }
        | Expr::Class { span, .. }
        | Expr::Given { span, .. }
        | Expr::Effect { span, .. }
        | Expr::Error { span, .. } => *span,
    }
}
