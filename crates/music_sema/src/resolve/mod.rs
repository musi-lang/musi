//! Two-pass name resolution.
//!
//! Pass 1: collect all top-level definitions (bindings, functions, types,
//! classes, effects) into the module scope.
//!
//! Pass 2: walk the full AST, resolving every name reference to a [`DefId`]
//! and creating child scopes for blocks, functions, and match arms.

#[cfg(test)]
mod tests;

mod pat;
mod ty;

use std::collections::HashMap;

use music_ast::decl::{ClassMember, EffectOp, ForeignDecl};
use music_ast::expr::{
    Arg, ArrayElem, BindKind, Expr, LetFields, MatchArm, Param, PwGuard, RecField,
};
use music_ast::ty::{Constraint, Ty, TyNamedRef, TyParam};
use music_ast::{AstArenas, ExprIdx, ParsedModule, PatIdx, TyIdx};
use music_shared::{DiagnosticBag, FileId, Interner, Span, Symbol};

use crate::def::{DefId, DefKind, DefTable};
use crate::error::SemaError;
use crate::scope::{ScopeId, ScopeTree};

/// Output accumulators from the resolution pass.
pub struct ResolveOutput {
    pub expr_defs: HashMap<ExprIdx, DefId>,
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

pub(super) struct Resolver<'a> {
    pub(super) ast: &'a AstArenas,
    pub(super) interner: &'a mut Interner,
    pub(super) file_id: FileId,
    pub(super) diags: &'a mut DiagnosticBag,
    pub(super) defs: &'a mut DefTable,
    pub(super) scopes: &'a mut ScopeTree,
    pub(super) output: ResolveOutput,
    pub(super) current_scope: ScopeId,
}

impl Resolver<'_> {
    fn collect_top_level(&mut self, expr_idx: ExprIdx) {
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

    /// Pass 2: resolve all name references in an expression.
    fn resolve_expr(&mut self, expr_idx: ExprIdx) {
        match self.ast.exprs[expr_idx].clone() {
            Expr::Name { name, span } => self.resolve_name(expr_idx, name, span),
            Expr::Lit { .. } | Expr::Error { .. } | Expr::Import { .. } | Expr::Export { .. } => {}
            Expr::Paren { inner, .. } | Expr::Annotated { inner, .. } => self.resolve_expr(inner),
            Expr::Choice { body, .. } => self.resolve_expr_choice(body),
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
                name,
                params,
                ops,
                exported,
                ..
            } => self.resolve_expr_effect(name, &params, &ops, exported),
            Expr::Foreign { decls, .. } => self.resolve_expr_foreign(&decls),
        }
    }

    fn resolve_name(&mut self, expr_idx: ExprIdx, name: Symbol, span: Span) {
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

    /// If `pat` is a function-like pattern (`Pat::Variant` with args),
    /// enters a child scope and defines all arg bindings as params.
    /// Returns `Some(parent_scope)` if a scope was entered, `None` otherwise.
    fn enter_fn_pat_scope(&mut self, pat: PatIdx) -> Option<ScopeId> {
        use music_ast::pat::Pat;
        if let Pat::Variant { args, .. } = &self.ast.pats[pat] {
            if args.is_empty() {
                return None;
            }
            let parent = self.current_scope;
            self.current_scope = self.scopes.push_child(parent);
            for &arg in args {
                self.define_pat(arg, DefKind::Param);
            }
            Some(parent)
        } else {
            None
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
                member_defs.push((sig.name, fn_id));
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
                ClassMember::Law { .. } => {}
            }
        }

        member_defs
    }

    fn resolve_expr_block(&mut self, stmts: &[ExprIdx], tail: Option<ExprIdx>) {
        let parent = self.current_scope;
        self.current_scope = self.scopes.push_child(parent);
        for &stmt in stmts {
            let fields = match &self.ast.exprs[stmt] {
                Expr::Binding { fields, .. } | Expr::Let { fields, .. } => Some(fields.clone()),
                _ => None,
            };
            if let Some(fields) = fields {
                self.resolve_block_binding(&fields);
            } else {
                self.resolve_expr(stmt);
            }
        }
        if let Some(t) = tail {
            self.resolve_expr(t);
        }
        self.current_scope = parent;
    }

    /// Resolves a block-local binding: pre-defines function names for recursion,
    /// resolves the value/type, then defines non-function patterns in scope.
    fn resolve_block_binding(&mut self, fields: &LetFields) {
        use music_ast::pat::Pat;
        let is_fn_pat = matches!(&self.ast.pats[fields.pat], Pat::Variant { .. });

        // For function-like patterns, pre-define the name to enable recursion.
        if is_fn_pat {
            self.define_fn_name(fields.pat, binding_def_kind(fields.kind));
        }

        let parent_ty_scope = if fields.params.is_empty() {
            None
        } else {
            Some(self.enter_ty_param_scope(&fields.params, &fields.constraints))
        };
        let fn_pat_parent = self.enter_fn_pat_scope(fields.pat);
        if let Some(v) = fields.value {
            self.resolve_expr(v);
        }
        if let Some(ty) = fields.ty {
            self.resolve_ty(ty);
        }
        if let Some(p) = fn_pat_parent {
            self.current_scope = p;
        }
        if let Some(p) = parent_ty_scope {
            self.current_scope = p;
        }
        if !is_fn_pat {
            self.define_pat(fields.pat, binding_def_kind(fields.kind));
        }
    }

    fn resolve_expr_call(&mut self, callee: ExprIdx, args: &[Arg]) {
        self.resolve_expr(callee);
        for arg in args {
            if let Arg::Pos { expr, .. } = arg {
                self.resolve_expr(*expr);
            }
        }
    }

    fn resolve_expr_let(&mut self, fields: &LetFields, body: Option<ExprIdx>) {
        use music_ast::pat::Pat;
        let is_fn_pat = matches!(&self.ast.pats[fields.pat], Pat::Variant { .. });

        let parent_ty_scope = if fields.params.is_empty() {
            None
        } else {
            Some(self.enter_ty_param_scope(&fields.params, &fields.constraints))
        };

        // For function-like patterns, pre-define the name for recursion.
        if is_fn_pat {
            self.define_fn_name(fields.pat, binding_def_kind(fields.kind));
        }

        let fn_pat_parent = self.enter_fn_pat_scope(fields.pat);

        if let Some(v) = fields.value {
            self.resolve_expr(v);
        }
        if let Some(ty) = fields.ty {
            self.resolve_ty(ty);
        }

        if let Some(p) = fn_pat_parent {
            self.current_scope = p;
        }

        if let Some(body) = body {
            let parent = self.current_scope;
            self.current_scope = self.scopes.push_child(parent);
            if !is_fn_pat {
                self.define_pat(fields.pat, binding_def_kind(fields.kind));
            }
            self.resolve_expr(body);
            self.current_scope = parent;
        } else if !is_fn_pat {
            self.define_pat(fields.pat, binding_def_kind(fields.kind));
        }

        if let Some(p) = parent_ty_scope {
            self.current_scope = p;
        }
    }

    fn resolve_expr_binding(&mut self, fields: &LetFields) {
        let parent_ty_scope = if fields.params.is_empty() {
            None
        } else {
            Some(self.enter_ty_param_scope(&fields.params, &fields.constraints))
        };

        let fn_pat_parent = self.enter_fn_pat_scope(fields.pat);

        if let Some(v) = fields.value {
            self.resolve_expr(v);
        }
        if let Some(ty) = fields.ty {
            self.resolve_ty(ty);
        }

        if let Some(p) = fn_pat_parent {
            self.current_scope = p;
        }

        if let Some(p) = parent_ty_scope {
            self.current_scope = p;
        }
    }

    fn resolve_expr_fn(&mut self, params: &[Param], ret_ty: Option<TyIdx>, body: ExprIdx) {
        let parent = self.current_scope;
        self.current_scope = self.scopes.push_child(parent);
        for param in params {
            let id = self.defs.alloc(param.name, DefKind::Param, param.span);
            self.defs.get_mut(id).param_mode = Some(param.mode);
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

    fn resolve_expr_match(&mut self, scrutinee: ExprIdx, arms: &[MatchArm]) {
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
        body: ExprIdx,
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

    fn resolve_expr_given(
        &mut self,
        target: &TyNamedRef,
        params: &[TyParam],
        constraints: &[Constraint],
        members: &[ClassMember],
    ) {
        let mut all_params: Vec<TyParam> = params.to_vec();
        for &arg in &target.args {
            self.collect_ty_var_nodes(arg, &mut all_params);
        }
        let parent = self.enter_ty_param_scope(&all_params, constraints);
        self.resolve_ty_named_ref(target);
        let _member_defs = self.resolve_class_members(members, None);
        self.current_scope = parent;
    }

    fn collect_ty_var_nodes(&self, ty_idx: TyIdx, out: &mut Vec<TyParam>) {
        match &self.ast.tys[ty_idx] {
            Ty::Var { name, span } => {
                if !out.iter().any(|p| p.name == *name) {
                    out.push(TyParam {
                        name: *name,
                        span: *span,
                    });
                }
            }
            Ty::Named { args, .. } => {
                for &arg in args {
                    self.collect_ty_var_nodes(arg, out);
                }
            }
            Ty::Option { inner, .. } | Ty::Ref { inner, .. } | Ty::Array { elem: inner, .. } => {
                self.collect_ty_var_nodes(*inner, out);
            }
            Ty::Fn { params, ret, .. } => {
                for &p in params {
                    self.collect_ty_var_nodes(p, out);
                }
                self.collect_ty_var_nodes(*ret, out);
            }
            Ty::Product { fields, .. }
            | Ty::Sum {
                variants: fields, ..
            } => {
                for &f in fields {
                    self.collect_ty_var_nodes(f, out);
                }
            }
            _ => {}
        }
    }

    fn resolve_expr_choice(&mut self, body: TyIdx) {
        let parent = self.current_scope;
        self.current_scope = self.scopes.push_child(parent);

        if let Ty::Sum { variants, .. } = &self.ast.tys[body] {
            for &variant_ty in variants {
                if let Ty::Named { name, .. } = &self.ast.tys[variant_ty] {
                    let id = self.defs.alloc(*name, DefKind::Variant, Span::DUMMY);
                    self.define_in_scope(*name, id, Span::DUMMY);
                }
            }
        }

        self.resolve_ty(body);
        self.current_scope = parent;
    }

    fn resolve_expr_effect(
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

    fn resolve_expr_foreign(&mut self, decls: &[ForeignDecl]) {
        for decl in decls {
            if let ForeignDecl::Fn { ty, .. } = decl {
                self.resolve_ty(*ty);
            }
        }
    }

    fn define_in_scope(&mut self, name: Symbol, def_id: DefId, span: Span) {
        if let Some(prev) = self.scopes.define(self.current_scope, name, def_id) {
            let prev_span = self.defs.get(prev).span;
            // Well-known placeholder (Span::DUMMY) being redefined by real user code — allow.
            if prev_span == Span::DUMMY && span != Span::DUMMY {
                return;
            }
            // Skip operator-sentinel symbols that the interner cannot resolve.
            if name == Symbol(u32::MAX) {
                return;
            }
            let name_str = self.interner.resolve(name);
            let d = self.diags.report(
                &SemaError::DuplicateDefinition {
                    name: Box::from(name_str),
                },
                span,
                self.file_id,
            );
            let _s = d.add_secondary(prev_span, self.file_id, "previous definition here");
        }
    }

    fn span_of_expr(&self, idx: ExprIdx) -> Span {
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
        | Expr::Choice { span, .. }
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
        | Expr::Foreign { span, .. }
        | Expr::Error { span, .. } => *span,
    }
}
