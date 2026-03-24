use std::collections::HashSet;

use music_ast::common::Param;
use music_ast::expr::{
    BinOp, ExprKind, FieldTarget, IndexKind, LetBinding, MatchArm, RecordField, SpliceKind, UnaryOp,
};
use music_ast::pat::PatKind;
use music_ast::ty::TyKind;
use music_ast::{ExprId, PatId, TyId};
use music_builtins::types::BuiltinType;
use music_db::Db;
use music_found::{Ident, Literal, Span, Symbol};
use music_resolve::def::{DefId, DefKind};
use music_resolve::queries::ResolutionMap;

use crate::dispatch::{method_index_for_op, resolve_binop};
use crate::effects;
use crate::env::{DispatchInfo, TypeEnv};
use crate::errors::{SemaError, SemaErrorKind};
use crate::types::{SemaTypeId, Ty};
use crate::unify::unify;

/// Wraps `Db` with type-checking state. Owns the `Db` during semantic
/// analysis and returns it via `finish()`.
pub struct SemaDb {
    pub db: Db,
    pub resolution: ResolutionMap,
    pub env: TypeEnv,
    pub errors: Vec<SemaError>,
    depth: u32,
    in_quote: bool,
    in_effectful: bool,
    mutable_defs: HashSet<DefId>,
    used_defs: HashSet<DefId>,
}

impl SemaDb {
    /// Creates a new `SemaDb` with a seeded type environment.
    #[must_use]
    pub fn new(db: Db, resolution: ResolutionMap) -> Self {
        let mut env = TypeEnv::new();
        env.seed_builtins();
        Self {
            db,
            resolution,
            env,
            errors: Vec::new(),
            depth: 0,
            in_quote: false,
            in_effectful: false,
            mutable_defs: HashSet::new(),
            used_defs: HashSet::new(),
        }
    }

    /// Type-checks all top-level expressions in the module.
    pub fn check_module(&mut self) {
        let root = self.db.ast.root.clone();
        for &expr_id in &root {
            let _ = self.synth(expr_id);
        }
        self.check_unused();
    }

    /// Emits errors for bindings/params that were never referenced.
    fn check_unused(&mut self) {
        let defs: Vec<_> = self
            .resolution
            .defs
            .iter()
            .map(|(id, info)| (id, info.name, info.kind, info.span))
            .collect();
        for (def_id, name, kind, span) in defs {
            if self.used_defs.contains(&def_id) {
                continue;
            }
            let name_str = self.db.interner.resolve(name);
            if name_str.starts_with('_') {
                continue;
            }
            match kind {
                DefKind::Value | DefKind::Function => {
                    self.errors.push(SemaError {
                        kind: SemaErrorKind::UnusedBinding { name },
                        span,
                        context: None,
                    });
                }
                _ => {}
            }
        }
    }

    /// Consumes the checker, returning the database, resolution map,
    /// type environment, and errors.
    #[must_use]
    pub fn finish(self) -> (Db, ResolutionMap, TypeEnv, Vec<SemaError>) {
        (self.db, self.resolution, self.env, self.errors)
    }

    /// Synthesizes a type for `expr_id` bottom-up, caching the result.
    fn synth(&mut self, expr_id: ExprId) -> SemaTypeId {
        if let Some(&ty) = self.env.type_map.get(&expr_id) {
            return ty;
        }

        let spanned = self.db.ast.exprs.get(expr_id);
        let span = spanned.span;
        let kind = spanned.kind.clone();

        let ty = match kind {
            ExprKind::Lit(ref lit) => self.synth_literal(lit),
            ExprKind::Var(ident) => self.synth_var(&ident),
            ExprKind::App(callee, ref args) => self.synth_app(callee, args, span),
            ExprKind::BinOp(op, lhs, rhs) => self.synth_binop(op, lhs, rhs, span),
            ExprKind::UnaryOp(op, operand) => self.synth_unary(op, operand),
            ExprKind::Branch {
                cond,
                then_br,
                else_br,
            } => self.synth_branch(cond, then_br, else_br, span),
            ExprKind::Let(ref binding) => self.synth_let(binding),
            ExprKind::Lambda {
                ref params,
                ret_ty,
                body,
            } => self.synth_lambda(params, ret_ty, body),
            ExprKind::Match(scrutinee, ref arms) => self.synth_match(scrutinee, arms, span),
            ExprKind::Seq(ref stmts) => self.synth_seq(stmts),
            ExprKind::TupleLit(ref elems) => self.synth_tuple(elems),
            ExprKind::ArrayLit(ref elems) => self.synth_array(elems, span),
            ExprKind::RecordLit(ref fields) => self.synth_record_lit(fields),
            ExprKind::VariantLit(ref _tag, ref args) => self.synth_variant_lit(args),
            ExprKind::Access {
                expr, ref field, ..
            } => self.synth_access(expr, field, span),
            ExprKind::Assign(lhs, rhs) => self.synth_assign(lhs, rhs, span),
            ExprKind::Return(val) | ExprKind::Resume(val) => {
                if let Some(v) = val {
                    let _ = self.synth(v);
                }
                self.env.intern(Ty::Never)
            }
            ExprKind::Need(e) => self.synth_need(e, span),
            ExprKind::Postfix { expr, .. } | ExprKind::TypeOp { expr, .. } => self.synth(expr),
            ExprKind::Index {
                expr,
                ref indices,
                kind,
            } => self.synth_index(expr, indices, kind, span),
            ExprKind::RecordDef(_)
            | ExprKind::ChoiceDef(_)
            | ExprKind::EffectDef(_)
            | ExprKind::ClassDef { .. }
            | ExprKind::InstanceDef(_) => self.env.intern(Ty::Builtin(BuiltinType::Type)),
            ExprKind::Handle { body, .. } => self.synth(body),
            ExprKind::FStrLit(_) => self.env.builtin(BuiltinType::String),
            ExprKind::RecordUpdate { base, .. } => self.synth(base),
            ExprKind::Comprehension { expr, .. } => {
                let elem_ty = self.synth(expr);
                self.env.intern(Ty::Array(elem_ty))
            }
            ExprKind::Quote(_) => self.synth_quote(),
            ExprKind::Splice(ref sk) => self.synth_splice(sk, span),
            ExprKind::Import { .. }
            | ExprKind::ForeignImport(_)
            | ExprKind::MatrixLit(_)
            | ExprKind::Piecewise(_) => self.env.intern(Ty::Any),
        };

        let _ = self.env.type_map.insert(expr_id, ty);
        ty
    }

    /// Checks `expr_id` against `expected`, unifying and reporting mismatches.
    fn check(&mut self, expr_id: ExprId, expected: SemaTypeId) -> SemaTypeId {
        let actual = self.synth(expr_id);
        let span = self.db.ast.exprs.get(expr_id).span;
        match unify(&mut self.env, actual, expected, span) {
            Ok(ty) => ty,
            Err(e) => {
                self.errors.push(e);
                actual
            }
        }
    }

    fn synth_literal(&self, lit: &Literal) -> SemaTypeId {
        match lit {
            Literal::Int(_) => self.env.builtin(BuiltinType::Int),
            Literal::Float(_) => self.env.builtin(BuiltinType::Float),
            Literal::Str(_) => self.env.builtin(BuiltinType::String),
            Literal::Rune(_) => self.env.builtin(BuiltinType::Rune),
        }
    }

    fn synth_var(&mut self, ident: &Ident) -> SemaTypeId {
        if let Some(def_id) = self.find_def_for_name(ident.name) {
            let _inserted = self.used_defs.insert(def_id);
            let def_info = self.resolution.defs.get(def_id);
            match def_info.kind {
                DefKind::Builtin(bt) => return self.env.intern(Ty::Builtin(bt)),
                DefKind::TypeClass => return self.env.intern(Ty::Class(ident.name)),
                DefKind::Effect => return self.env.intern(Ty::Effect(ident.name)),
                DefKind::TypeParam => return self.env.intern(Ty::Param(ident.name)),
                _ => {}
            }
        }
        if let Some(ty) = self.find_let_value_type(ident.name) {
            return ty;
        }
        self.env.intern(Ty::Any)
    }

    fn find_let_value_type(&self, name: Symbol) -> Option<SemaTypeId> {
        for eid in self.env.type_map.keys() {
            let spanned = self.db.ast.exprs.get(*eid);
            if let ExprKind::Let(binding) = &spanned.kind {
                let pat = self.db.ast.pats.get(binding.pat);
                if let PatKind::Bind(bind_ident) = &pat.kind {
                    if bind_ident.name == name {
                        if let Some(value_id) = binding.value {
                            return self.env.type_map.get(&value_id).copied();
                        }
                    }
                }
            }
        }
        None
    }

    fn find_def_for_name(&self, name: Symbol) -> Option<DefId> {
        for (def_id, info) in &self.resolution.defs {
            if info.name == name {
                return Some(def_id);
            }
        }
        None
    }

    fn synth_app(&mut self, callee: ExprId, args: &[ExprId], span: Span) -> SemaTypeId {
        let callee_ty = self.synth(callee);
        let resolved = self.env.resolve_var(callee_ty);
        let ty = self.env.types.get(resolved).clone();

        match ty {
            Ty::Arrow { param, ret } => {
                if args.is_empty() {
                    let param_resolved = self.env.resolve_var(param);
                    let param_ty = self.env.types.get(param_resolved).clone();
                    if !matches!(param_ty, Ty::Unit) {
                        self.errors.push(SemaError {
                            kind: SemaErrorKind::ArityMismatch {
                                expected: 1,
                                found: 0,
                            },
                            span,
                            context: None,
                        });
                    }
                } else if args.len() == 1 {
                    let _ = self.check(args[0], param);
                } else {
                    let param_resolved = self.env.resolve_var(param);
                    let param_ty = self.env.types.get(param_resolved).clone();
                    match param_ty {
                        Ty::Tuple(ref elems) if elems.len() == args.len() => {
                            for (i, &arg) in args.iter().enumerate() {
                                let _ = self.check(arg, elems[i]);
                            }
                        }
                        _ => {
                            self.errors.push(SemaError {
                                kind: SemaErrorKind::ArityMismatch {
                                    expected: 1,
                                    found: args.len(),
                                },
                                span,
                                context: None,
                            });
                            for &arg in args {
                                let _ = self.synth(arg);
                            }
                        }
                    }
                }
                ret
            }
            Ty::Any => {
                for &arg in args {
                    let _ = self.synth(arg);
                }
                self.env.intern(Ty::Any)
            }
            _ => {
                self.errors.push(SemaError {
                    kind: SemaErrorKind::NotCallable,
                    span,
                    context: None,
                });
                for &arg in args {
                    let _ = self.synth(arg);
                }
                self.env.intern(Ty::Any)
            }
        }
    }

    fn synth_binop(&mut self, op: BinOp, lhs: ExprId, rhs: ExprId, span: Span) -> SemaTypeId {
        let lhs_ty = self.synth(lhs);
        let rhs_ty = self.synth(rhs);

        let resolution = resolve_binop(&mut self.env, op, lhs_ty, rhs_ty);

        let dispatch = if let Some(class_name) = resolution.needs_class {
            self.resolution
                .defs
                .iter()
                .find(|(_, info)| self.db.interner.resolve(info.name) == class_name)
                .map(|(_, info)| DispatchInfo::Dictionary {
                    class: info.name,
                    method_idx: method_index_for_op(op),
                })
        } else {
            resolution.dispatch
        };

        if let Some(info) = dispatch {
            let _ = self.env.dispatch.insert(lhs, info);
        }

        if matches!(
            op,
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem
        ) {
            if let Err(e) = unify(&mut self.env, lhs_ty, rhs_ty, span) {
                self.errors.push(e);
            }
        }

        resolution.result_ty
    }

    fn synth_unary(&mut self, op: UnaryOp, operand: ExprId) -> SemaTypeId {
        let operand_ty = self.synth(operand);
        match op {
            UnaryOp::Neg | UnaryOp::Spread => operand_ty,
            UnaryOp::Not => {
                let resolved = self.env.resolve_var(operand_ty);
                let ty = self.env.types.get(resolved).clone();
                match ty {
                    Ty::Builtin(BuiltinType::Bool) => self.env.builtin(BuiltinType::Bool),
                    _ => operand_ty,
                }
            }
            UnaryOp::Mut => {
                let inner = self.env.resolve_var(operand_ty);
                self.env.intern(Ty::Mut(inner))
            }
        }
    }

    fn synth_branch(
        &mut self,
        cond: ExprId,
        then_br: ExprId,
        else_br: ExprId,
        span: Span,
    ) -> SemaTypeId {
        let cond_ty = self.synth(cond);
        let bool_ty = self.env.builtin(BuiltinType::Bool);
        if let Err(e) = unify(&mut self.env, cond_ty, bool_ty, span) {
            self.errors.push(e);
        }

        let then_ty = self.synth(then_br);
        let else_ty = self.synth(else_br);

        match unify(&mut self.env, then_ty, else_ty, span) {
            Ok(ty) => ty,
            Err(e) => {
                self.errors.push(e);
                then_ty
            }
        }
    }

    fn synth_let(&mut self, binding: &LetBinding) -> SemaTypeId {
        let span = binding
            .value
            .map_or_else(|| Span::new(0, 0), |v| self.db.ast.exprs.get(v).span);

        // #1: export only at top level
        if binding.modifiers.exported && self.depth > 0 {
            self.errors.push(SemaError {
                kind: SemaErrorKind::ExportNotTopLevel,
                span,
                context: None,
            });
        }

        // #2: opaque requires export
        if binding.modifiers.opaque && !binding.modifiers.exported {
            self.errors.push(SemaError {
                kind: SemaErrorKind::OpaqueWithoutExport,
                span,
                context: None,
            });
        }

        // #3: foreign only at top level
        if binding.modifiers.foreign_abi.is_some() && self.depth > 0 {
            self.errors.push(SemaError {
                kind: SemaErrorKind::ForeignNotTopLevel,
                span,
                context: None,
            });
        }

        // #6: track mutable bindings
        if binding.modifiers.mutable {
            let pat = self.db.ast.pats.get(binding.pat);
            if let PatKind::Bind(bind_ident) = &pat.kind {
                if let Some(def_id) = self.find_def_for_name(bind_ident.name) {
                    let _inserted = self.mutable_defs.insert(def_id);
                }
            }
        }

        let val_ty = binding.value.map(|value| self.synth(value));

        if let Some(ref sig) = binding.sig {
            // Unify body type against declared return type
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

            // For function bindings with params, register an Arrow type
            if !sig.params.is_empty() {
                let param_ty = if sig.params.len() == 1 {
                    match sig.params[0].ty {
                        Some(t) => self.lower_ty(t),
                        None => self.env.fresh_var(),
                    }
                } else {
                    let pts: Vec<SemaTypeId> = sig
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
                let arrow = self.env.intern(Ty::Arrow {
                    param: param_ty,
                    ret: ret_ty,
                });
                if let Some(value_id) = binding.value {
                    let _ = self.env.type_map.insert(value_id, arrow);
                }
            }
        }

        self.env.intern(Ty::Unit)
    }

    /// Converts an AST `TyId` to a semantic `SemaTypeId`.
    fn lower_ty(&mut self, ty_id: TyId) -> SemaTypeId {
        let ty_kind = self.db.ast.types.get(ty_id).kind.clone();
        match ty_kind {
            TyKind::Named { name, .. } => {
                if let Some(&def_id) = self.resolution.ty_res.get(&ty_id) {
                    let def = self.resolution.defs.get(def_id);
                    match def.kind {
                        DefKind::Builtin(bt) => self.env.builtin(bt),
                        _ => self.env.intern(Ty::Param(name.name)),
                    }
                } else {
                    self.env.intern(Ty::Any)
                }
            }
            TyKind::Arrow { from, to } => {
                let from_ty = self.lower_ty(from);
                let to_ty = self.lower_ty(to);
                self.env.intern(Ty::Arrow {
                    param: from_ty,
                    ret: to_ty,
                })
            }
            TyKind::EffectArrow { from, to } => {
                let from_ty = self.lower_ty(from);
                let to_ty = self.lower_ty(to);
                self.env.intern(Ty::EffectArrow {
                    param: from_ty,
                    ret: to_ty,
                    effects: Vec::new(),
                })
            }
            TyKind::Tuple(elems) => {
                let elem_tys: Vec<_> = elems.iter().map(|&t| self.lower_ty(t)).collect();
                self.env.intern(Ty::Tuple(elem_tys))
            }
            TyKind::Union(members) => {
                let member_tys: Vec<_> = members.iter().map(|&t| self.lower_ty(t)).collect();
                self.env.intern(Ty::Union(member_tys))
            }
            TyKind::Mut(inner) => {
                let inner_ty = self.lower_ty(inner);
                self.env.intern(Ty::Mut(inner_ty))
            }
            TyKind::Option(inner) => {
                let inner_ty = self.lower_ty(inner);
                let unit_ty = self.env.intern(Ty::Unit);
                self.env.intern(Ty::Union(vec![inner_ty, unit_ty]))
            }
            TyKind::Array { elem, .. } => {
                let elem_ty = self.lower_ty(elem);
                self.env.intern(Ty::Array(elem_ty))
            }
            TyKind::Pi { ret_ty, .. } => self.lower_ty(ret_ty),
        }
    }

    fn synth_lambda(&mut self, params: &[Param], ret_ty: Option<TyId>, body: ExprId) -> SemaTypeId {
        let param_ty = if params.len() == 1 {
            self.env.fresh_var()
        } else {
            let param_tys: Vec<SemaTypeId> = params.iter().map(|_| self.env.fresh_var()).collect();
            self.env.intern(Ty::Tuple(param_tys))
        };

        self.depth = self.depth.saturating_add(1);
        let prev_effectful = self.in_effectful;
        if let Some(ty_id) = ret_ty {
            if self.is_effect_arrow_context(ty_id) {
                self.in_effectful = true;
            }
        }

        let body_ty = self.synth(body);

        self.in_effectful = prev_effectful;
        self.depth = self.depth.saturating_sub(1);

        // Determine if the lambda's declared return type uses a pure arrow
        let is_declared_pure = ret_ty.is_none_or(|ty_id| !self.is_effect_arrow_context(ty_id));

        let arrow_ty = self.env.intern(Ty::Arrow {
            param: param_ty,
            ret: body_ty,
        });

        if is_declared_pure {
            let body_effects = self.collect_need_exprs(body);
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

    /// Checks whether a `TyId` is part of an effect arrow context.
    fn is_effect_arrow_context(&self, ty_id: TyId) -> bool {
        // Walk up from the ret_ty to see if this lambda was declared with `~>`
        // For now, check if the type itself is an EffectArrow
        let kind = &self.db.ast.types.get(ty_id).kind;
        matches!(kind, TyKind::EffectArrow { .. })
    }

    /// Collects `SemaTypeId`s of `Need` expressions found directly in a body.
    fn collect_need_exprs(&self, expr_id: ExprId) -> Vec<SemaTypeId> {
        let mut effects = Vec::new();
        self.walk_for_needs(expr_id, &mut effects);
        effects
    }

    fn walk_for_needs(&self, expr_id: ExprId, out: &mut Vec<SemaTypeId>) {
        let kind = &self.db.ast.exprs.get(expr_id).kind;
        match kind {
            ExprKind::Need(inner) => {
                if let Some(&ty) = self.env.type_map.get(inner) {
                    out.push(ty);
                } else {
                    out.push(self.env.builtin(BuiltinType::Type));
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
            ExprKind::Match(scrutinee, arms) => {
                self.walk_for_needs(*scrutinee, out);
                for arm in arms {
                    self.walk_for_needs(arm.body, out);
                }
            }
            // Nested lambdas and other nodes: don't recurse (own purity scope)
            _ => {}
        }
    }

    /// Extracts a symbol from an effect type for error reporting.
    fn effect_symbol(&mut self, ty_id: SemaTypeId) -> Symbol {
        let resolved = self.env.resolve_var(ty_id);
        let ty = self.env.types.get(resolved);
        match ty {
            Ty::Effect(sym) | Ty::Class(sym) | Ty::Param(sym) => *sym,
            _ => self.db.interner.intern("_"),
        }
    }

    fn synth_match(&mut self, scrutinee: ExprId, arms: &[MatchArm], span: Span) -> SemaTypeId {
        let _ = self.synth(scrutinee);

        if arms.is_empty() {
            return self.env.intern(Ty::Never);
        }

        let mut wildcard_seen = false;
        let mut result_ty = self.env.intern(Ty::Never);

        for (i, arm) in arms.iter().enumerate() {
            if wildcard_seen {
                let arm_span = self.db.ast.exprs.get(arm.body).span;
                self.errors.push(SemaError {
                    kind: SemaErrorKind::UnreachablePattern,
                    span: arm_span,
                    context: None,
                });
            }

            self.check_or_pattern_bindings(arm.pat);

            let pat_kind = &self.db.ast.pats.get(arm.pat).kind;
            if matches!(pat_kind, PatKind::Wildcard | PatKind::Bind(_)) {
                wildcard_seen = true;
            }

            let arm_ty = self.synth(arm.body);
            if i == 0 {
                result_ty = arm_ty;
            } else {
                match unify(&mut self.env, result_ty, arm_ty, span) {
                    Ok(ty) => result_ty = ty,
                    Err(e) => self.errors.push(e),
                }
            }
        }

        result_ty
    }

    fn collect_pat_bindings(&self, pat_id: PatId, names: &mut HashSet<Symbol>) {
        let pat = self.db.ast.pats.get(pat_id);
        match &pat.kind {
            PatKind::Bind(ident) => {
                let _inserted = names.insert(ident.name);
            }
            PatKind::As { name, pat } => {
                let _inserted = names.insert(name.name);
                self.collect_pat_bindings(*pat, names);
            }
            PatKind::Variant { fields, .. } => {
                for &f in fields {
                    self.collect_pat_bindings(f, names);
                }
            }
            PatKind::Record(fields) => {
                for f in fields {
                    if let Some(p) = f.pat {
                        self.collect_pat_bindings(p, names);
                    } else {
                        let _inserted = names.insert(f.name.name);
                    }
                }
            }
            PatKind::Tuple(pats) | PatKind::Array(pats) | PatKind::Or(pats) => {
                for &p in pats {
                    self.collect_pat_bindings(p, names);
                }
            }
            PatKind::Wildcard | PatKind::Lit(_) => {}
        }
    }

    fn check_or_pattern_bindings(&mut self, pat_id: PatId) {
        let pat = self.db.ast.pats.get(pat_id);
        if let PatKind::Or(alts) = &pat.kind {
            let alts = alts.clone();
            if alts.len() < 2 {
                return;
            }
            let mut first_names = HashSet::new();
            self.collect_pat_bindings(alts[0], &mut first_names);

            for &alt in &alts[1..] {
                let mut alt_names = HashSet::new();
                self.collect_pat_bindings(alt, &mut alt_names);
                if alt_names != first_names {
                    let span = self.db.ast.pats.get(alt).span;
                    self.errors.push(SemaError {
                        kind: SemaErrorKind::OrPatternMismatch,
                        span,
                        context: None,
                    });
                }
            }
        }
    }

    fn synth_seq(&mut self, stmts: &[ExprId]) -> SemaTypeId {
        if stmts.is_empty() {
            return self.env.intern(Ty::Unit);
        }

        let mut last_ty = self.env.intern(Ty::Unit);
        let mut diverged = false;
        for &stmt in stmts {
            if diverged {
                let stmt_span = self.db.ast.exprs.get(stmt).span;
                self.errors.push(SemaError {
                    kind: SemaErrorKind::UnreachableCode,
                    span: stmt_span,
                    context: None,
                });
            }
            last_ty = self.synth(stmt);
            let resolved = self.env.resolve_var(last_ty);
            if matches!(self.env.types.get(resolved), Ty::Never) {
                diverged = true;
            }
        }
        last_ty
    }

    fn synth_tuple(&mut self, elems: &[ExprId]) -> SemaTypeId {
        let elem_tys: Vec<SemaTypeId> = elems.iter().map(|&e| self.synth(e)).collect();
        self.env.intern(Ty::Tuple(elem_tys))
    }

    fn synth_array(&mut self, elems: &[ExprId], span: Span) -> SemaTypeId {
        if elems.is_empty() {
            let var = self.env.fresh_var();
            return self.env.intern(Ty::Array(var));
        }

        let mut elem_ty = self.synth(elems[0]);
        for &elem in &elems[1..] {
            let ty = self.synth(elem);
            match unify(&mut self.env, elem_ty, ty, span) {
                Ok(unified) => elem_ty = unified,
                Err(e) => self.errors.push(e),
            }
        }

        self.env.intern(Ty::Array(elem_ty))
    }

    fn synth_record_lit(&mut self, fields: &[RecordField]) -> SemaTypeId {
        let mut field_types = Vec::new();
        for field in fields {
            match field {
                RecordField::Named { name, value } => {
                    let ty = if let Some(v) = value {
                        self.synth(*v)
                    } else {
                        self.env.intern(Ty::Any)
                    };
                    field_types.push((name.name, ty));
                }
                RecordField::Spread(expr) => {
                    let _ = self.synth(*expr);
                }
            }
        }
        self.env.intern(Ty::Record {
            fields: field_types,
        })
    }

    fn synth_variant_lit(&mut self, args: &[ExprId]) -> SemaTypeId {
        for &arg in args {
            let _ = self.synth(arg);
        }
        self.env.intern(Ty::Any)
    }

    fn synth_access(&mut self, expr: ExprId, field: &FieldTarget, span: Span) -> SemaTypeId {
        let base_ty = self.synth(expr);
        let resolved = self.env.resolve_var(base_ty);
        let ty = self.env.types.get(resolved).clone();

        match (&ty, field) {
            (Ty::Record { fields }, FieldTarget::Name(ident)) => {
                for &(name, field_ty) in fields {
                    if name == ident.name {
                        return field_ty;
                    }
                }
                self.errors.push(SemaError {
                    kind: SemaErrorKind::UndefinedField { field: ident.name },
                    span,
                    context: None,
                });
                self.env.intern(Ty::Any)
            }
            (Ty::Tuple(elems), FieldTarget::Index(idx)) => {
                let i = usize::try_from(*idx).expect("u32 fits in usize");
                if i < elems.len() {
                    elems[i]
                } else {
                    self.errors.push(SemaError {
                        kind: SemaErrorKind::ArityMismatch {
                            expected: elems.len(),
                            found: i + 1,
                        },
                        span,
                        context: Some("tuple index"),
                    });
                    self.env.intern(Ty::Any)
                }
            }
            (Ty::Any | Ty::Unknown, _) => self.env.intern(Ty::Any),
            (Ty::Var(_), _) => self.env.fresh_var(),
            (_, FieldTarget::Name(ident)) => {
                self.errors.push(SemaError {
                    kind: SemaErrorKind::UndefinedField { field: ident.name },
                    span,
                    context: None,
                });
                self.env.intern(Ty::Any)
            }
            (_, FieldTarget::Index(_)) => {
                self.errors.push(SemaError {
                    kind: SemaErrorKind::NotIndexable,
                    span,
                    context: None,
                });
                self.env.intern(Ty::Any)
            }
        }
    }

    fn synth_need(&mut self, e: ExprId, span: Span) -> SemaTypeId {
        if !self.in_effectful {
            self.errors.push(SemaError {
                kind: SemaErrorKind::PurityViolation {
                    effect: self.db.interner.intern("_"),
                },
                span,
                context: Some("need outside effectful function"),
            });
        }
        self.synth(e)
    }

    fn synth_quote(&mut self) -> SemaTypeId {
        let prev = self.in_quote;
        self.in_quote = true;
        let ty = self.env.intern(Ty::Any);
        self.in_quote = prev;
        ty
    }

    fn synth_splice(&mut self, sk: &SpliceKind, span: Span) -> SemaTypeId {
        if !self.in_quote {
            self.errors.push(SemaError {
                kind: SemaErrorKind::SpliceOutsideQuote,
                span,
                context: None,
            });
        }
        match sk {
            SpliceKind::Expr(e) => {
                let _ = self.synth(*e);
            }
            SpliceKind::Array(es) => {
                for &e in es {
                    let _ = self.synth(e);
                }
            }
            SpliceKind::Ident(_) => {}
        }
        self.env.intern(Ty::Any)
    }

    fn synth_index(
        &mut self,
        expr: ExprId,
        indices: &[ExprId],
        kind: IndexKind,
        span: Span,
    ) -> SemaTypeId {
        let base_ty = self.synth(expr);
        for &idx in indices {
            let _ = self.synth(idx);
        }

        let resolved = self.env.resolve_var(base_ty);
        let ty = self.env.types.get(resolved).clone();

        match ty {
            Ty::Array(elem) | Ty::List(elem) => match kind {
                IndexKind::Point => elem,
                IndexKind::Slice => self.env.intern(Ty::Array(elem)),
            },
            Ty::Tuple(ref elems) => {
                if indices.len() == 1 {
                    let idx_ty = self.synth(indices[0]);
                    let idx_resolved = self.env.resolve_var(idx_ty);
                    let idx_val = self.env.types.get(idx_resolved).clone();
                    if idx_val == Ty::Builtin(BuiltinType::Int) {
                        if let Some(first) = elems.first() {
                            return *first;
                        }
                    }
                }
                self.env.intern(Ty::Any)
            }
            Ty::Any | Ty::Unknown | Ty::Var(_) => self.env.intern(Ty::Any),
            _ => {
                self.errors.push(SemaError {
                    kind: SemaErrorKind::NotIndexable,
                    span,
                    context: None,
                });
                self.env.intern(Ty::Any)
            }
        }
    }

    fn synth_assign(&mut self, lhs: ExprId, rhs: ExprId, span: Span) -> SemaTypeId {
        let lhs_kind = self.db.ast.exprs.get(lhs).kind.clone();

        match &lhs_kind {
            ExprKind::Var(ident) => {
                if let Some(def_id) = self.find_def_for_name(ident.name) {
                    if !self.mutable_defs.contains(&def_id) {
                        self.errors.push(SemaError {
                            kind: SemaErrorKind::MutabilityViolation,
                            span,
                            context: None,
                        });
                    }
                }
            }
            ExprKind::Access { .. } | ExprKind::Index { .. } => {}
            _ => {
                self.errors.push(SemaError {
                    kind: SemaErrorKind::InvalidAssignTarget,
                    span,
                    context: None,
                });
            }
        }

        let lhs_ty = self.synth(lhs);
        let rhs_ty = self.synth(rhs);

        if let Err(e) = unify(&mut self.env, lhs_ty, rhs_ty, span) {
            self.errors.push(e);
        }

        self.env.intern(Ty::Unit)
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
