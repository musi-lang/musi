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

use music_ast::decl::ClassMember;
use music_ast::expr::{Arg, ArrayElem, BindKind, Expr, PwGuard, RecField};
use music_ast::pat::Pat;
use music_ast::ty::{Ty, TyNamedRef};
use music_ast::{AstArenas, Stmt};
use music_shared::{DiagnosticBag, FileId, Idx, Interner, Span};

use crate::def::{DefId, DefKind, DefTable};
use crate::error::SemaError;
use crate::scope::{ScopeId, ScopeTree};

/// Result of the resolution pass.
pub struct ResolveResult {
    pub defs: DefTable,
    pub scopes: ScopeTree,
    pub expr_defs: HashMap<Idx<Expr>, DefId>,
    pub pat_defs: HashMap<Span, DefId>,
    pub root_scope: ScopeId,
}

/// Runs two-pass name resolution over a list of top-level statements.
#[must_use]
#[allow(clippy::too_many_arguments)]
pub fn resolve(
    stmts: &[Stmt],
    ast: &AstArenas,
    interner: &mut Interner,
    file_id: FileId,
    diags: &mut DiagnosticBag,
    defs: &mut DefTable,
    scopes: &mut ScopeTree,
    module_scope: ScopeId,
) -> ResolveResult {
    let mut resolver = Resolver {
        ast,
        interner,
        file_id,
        diags,
        defs,
        scopes,
        expr_defs: HashMap::new(),
        pat_defs: HashMap::new(),
        current_scope: module_scope,
    };

    // Pass 1: collect top-level definitions.
    for stmt in stmts {
        resolver.collect_top_level(stmt.expr);
    }

    // Pass 2: resolve all references.
    for stmt in stmts {
        resolver.resolve_expr(stmt.expr);
    }

    ResolveResult {
        defs: DefTable::new(), // placeholder — the real defs are in the shared table
        scopes: ScopeTree::new(),
        expr_defs: resolver.expr_defs,
        pat_defs: resolver.pat_defs,
        root_scope: module_scope,
    }
}

struct Resolver<'a> {
    ast: &'a AstArenas,
    interner: &'a mut Interner,
    file_id: FileId,
    diags: &'a mut DiagnosticBag,
    defs: &'a mut DefTable,
    scopes: &'a mut ScopeTree,
    expr_defs: HashMap<Idx<Expr>, DefId>,
    pat_defs: HashMap<Span, DefId>,
    current_scope: ScopeId,
}

impl Resolver<'_> {
    /// Pass 1: register top-level definitions without descending into bodies.
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
    #[allow(clippy::too_many_lines)]
    fn resolve_expr(&mut self, expr_idx: Idx<Expr>) {
        match self.ast.exprs[expr_idx].clone() {
            Expr::Name { name, span } => {
                if let Some(def_id) = self.scopes.lookup(self.current_scope, name) {
                    let _prev = self.expr_defs.insert(expr_idx, def_id);
                    self.defs.get_mut(def_id).use_count += 1;
                } else {
                    let name_str = self.interner.resolve(name);
                    let _d = self.diags.report(
                        &SemaError::UndefinedName {
                            name: Box::from(name_str),
                        },
                        span,
                        self.file_id,
                    );
                }
            }
            Expr::Lit { .. } | Expr::Error { .. } | Expr::Import { .. } | Expr::Export { .. } => {}
            Expr::Paren { inner, .. } => self.resolve_expr(inner),
            Expr::Tuple { elems, .. } => {
                for &elem in &elems {
                    self.resolve_expr(elem);
                }
            }
            Expr::Block { stmts, tail, .. } => {
                let parent = self.current_scope;
                self.current_scope = self.scopes.push_child(parent);
                for &stmt in &stmts {
                    self.resolve_expr(stmt);
                }
                if let Some(t) = tail {
                    self.resolve_expr(t);
                }
                self.current_scope = parent;
            }
            Expr::Let { fields, body, .. } => {
                // Value first (evaluated in outer scope).
                self.resolve_expr(fields.value);
                if let Some(ty) = fields.ty {
                    self.resolve_ty(ty);
                }

                // If there is a body (let-in), create a child scope.
                if let Some(body) = body {
                    let parent = self.current_scope;
                    self.current_scope = self.scopes.push_child(parent);
                    self.define_pat(fields.pat, binding_def_kind(fields.kind));
                    self.resolve_expr(body);
                    self.current_scope = parent;
                } else {
                    // Statement-level let: define in current scope.
                    self.define_pat(fields.pat, binding_def_kind(fields.kind));
                }
            }
            Expr::Binding { fields, .. } => {
                self.resolve_expr(fields.value);
                if let Some(ty) = fields.ty {
                    self.resolve_ty(ty);
                }
                // Already defined in pass 1 for top-level; re-defining is harmless.
                self.define_pat(fields.pat, binding_def_kind(fields.kind));
            }
            Expr::Fn {
                params,
                ret_ty,
                body,
                ..
            } => {
                let parent = self.current_scope;
                self.current_scope = self.scopes.push_child(parent);
                for param in &params {
                    let id = self.defs.alloc(param.name, DefKind::Param, param.span);
                    self.define_in_scope(param.name, id, param.span);
                    let _inserted = self.pat_defs.insert(param.span, id);
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
            Expr::Call { callee, args, .. } => {
                self.resolve_expr(callee);
                for arg in &args {
                    match arg {
                        Arg::Pos { expr, .. } => self.resolve_expr(*expr),
                        Arg::Hole { .. } => {}
                    }
                }
            }
            Expr::BinOp { left, right, .. } => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            }
            Expr::UnaryOp { operand, .. } => {
                self.resolve_expr(operand);
            }
            Expr::Field { object, .. } => {
                self.resolve_expr(object);
            }
            Expr::Index { object, index, .. } => {
                self.resolve_expr(object);
                self.resolve_expr(index);
            }
            Expr::Update { base, fields, .. } => {
                self.resolve_expr(base);
                for field in &fields {
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
            Expr::Record { fields, .. } => {
                for field in &fields {
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
            Expr::Array { elems, .. } => {
                for elem in &elems {
                    match elem {
                        ArrayElem::Elem { expr, .. } | ArrayElem::Spread { expr, .. } => {
                            self.resolve_expr(*expr);
                        }
                    }
                }
            }
            Expr::Variant { args, .. } => {
                for &arg in &args {
                    self.resolve_expr(arg);
                }
            }
            Expr::Piecewise { arms, .. } => {
                for arm in &arms {
                    match arm.guard {
                        PwGuard::When { expr, .. } => {
                            self.resolve_expr(expr);
                        }
                        PwGuard::Any { .. } => {}
                    }
                    self.resolve_expr(arm.result);
                }
            }
            Expr::Match {
                scrutinee, arms, ..
            } => {
                self.resolve_expr(scrutinee);
                for arm in &arms {
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
            Expr::Return { value, .. } => {
                if let Some(v) = value {
                    self.resolve_expr(v);
                }
            }
            Expr::Quantified {
                body,
                params,
                constraints,
                ..
            } => {
                let parent = self.current_scope;
                self.current_scope = self.scopes.push_child(parent);
                for param in &params {
                    let id = self.defs.alloc(param.name, DefKind::Type, param.span);
                    self.define_in_scope(param.name, id, param.span);
                }
                for constraint in &constraints {
                    self.resolve_ty_named_ref(&constraint.bound);
                }
                self.resolve_expr(body);
                self.current_scope = parent;
            }
            Expr::Class {
                name,
                params,
                constraints,
                members,
                ..
            } => {
                let parent = self.current_scope;
                self.current_scope = self.scopes.push_child(parent);
                for param in &params {
                    let id = self.defs.alloc(param.name, DefKind::Type, param.span);
                    self.define_in_scope(param.name, id, param.span);
                }
                for constraint in &constraints {
                    self.resolve_ty_named_ref(&constraint.bound);
                }
                for member in &members {
                    match member {
                        ClassMember::Fn { sig, default, .. } => {
                            let fn_id = self.defs.alloc(sig.name, DefKind::Fn, sig.span);
                            // Look up the class DefId
                            if let Some(class_def) = self.scopes.lookup(parent, name) {
                                self.defs.get_mut(fn_id).parent = Some(class_def);
                            }
                            if let Some(default_body) = default {
                                self.resolve_expr(*default_body);
                            }
                        }
                        ClassMember::Law { body, .. } => {
                            self.resolve_expr(*body);
                        }
                    }
                }
                self.current_scope = parent;
            }
            Expr::Given {
                target,
                params,
                constraints,
                members,
                ..
            } => {
                self.resolve_ty_named_ref(&target);
                let parent = self.current_scope;
                self.current_scope = self.scopes.push_child(parent);
                for param in &params {
                    let id = self.defs.alloc(param.name, DefKind::Type, param.span);
                    self.define_in_scope(param.name, id, param.span);
                }
                for constraint in &constraints {
                    self.resolve_ty_named_ref(&constraint.bound);
                }
                for member in &members {
                    match member {
                        ClassMember::Fn { sig, default, .. } => {
                            let _fn_id = self.defs.alloc(sig.name, DefKind::Fn, sig.span);
                            if let Some(default_body) = default {
                                self.resolve_expr(*default_body);
                            }
                        }
                        ClassMember::Law { body, .. } => {
                            self.resolve_expr(*body);
                        }
                    }
                }
                self.current_scope = parent;
            }
            Expr::Effect {
                name, params, ops, ..
            } => {
                let parent = self.current_scope;
                self.current_scope = self.scopes.push_child(parent);
                for param in &params {
                    let id = self.defs.alloc(param.name, DefKind::Type, param.span);
                    self.define_in_scope(param.name, id, param.span);
                }
                // Look up the effect DefId from parent scope
                let effect_def = self.scopes.lookup(parent, name);
                for op in &ops {
                    let op_id = self.defs.alloc(op.name, DefKind::EffectOp, op.span);
                    if let Some(eff) = effect_def {
                        self.defs.get_mut(op_id).parent = Some(eff);
                    }
                    self.resolve_ty(op.ty);
                }
                self.current_scope = parent;
            }
            Expr::Annotated { inner, .. } => {
                self.resolve_expr(inner);
            }
        }
    }

    /// Resolve names in a type annotation.
    fn resolve_ty(&mut self, ty_idx: Idx<Ty>) {
        match self.ast.tys[ty_idx].clone() {
            Ty::Named { name, args, span } => {
                if self.scopes.lookup(self.current_scope, name).is_none() {
                    let name_str = self.interner.resolve(name);
                    let _d = self.diags.report(
                        &SemaError::UndefinedName {
                            name: Box::from(name_str),
                        },
                        span,
                        self.file_id,
                    );
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
                let parent = self.current_scope;
                self.current_scope = self.scopes.push_child(parent);
                for param in &params {
                    let id = self.defs.alloc(param.name, DefKind::Type, param.span);
                    self.define_in_scope(param.name, id, param.span);
                }
                for constraint in &constraints {
                    self.resolve_ty_named_ref(&constraint.bound);
                }
                self.resolve_ty(body);
                self.current_scope = parent;
            }
            Ty::Var { .. } | Ty::Error { .. } => {}
        }
    }

    fn resolve_ty_named_ref(&mut self, named: &TyNamedRef) {
        if self.scopes.lookup(self.current_scope, named.name).is_none() {
            let name_str = self.interner.resolve(named.name);
            let _d = self.diags.report(
                &SemaError::UndefinedName {
                    name: Box::from(name_str),
                },
                named.span,
                self.file_id,
            );
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
                let _inserted = self.pat_defs.insert(span, id);
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
                let _inserted = self.pat_defs.insert(span, id);
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

    fn define_in_scope(&mut self, name: music_shared::Symbol, def_id: DefId, span: Span) {
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
