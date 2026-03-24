use music_ast::ExprId;
use music_ast::common::Param;
use music_ast::expr::{BinOp, ExprKind, FieldTarget, LetBinding, MatchArm, RecordField, UnaryOp};
use music_ast::pat::PatKind;
use music_builtins::types::BuiltinType;
use music_db::Db;
use music_found::{Ident, Literal, Span, Symbol};
use music_resolve::def::{DefId, DefKind};
use music_resolve::queries::ResolutionMap;

use crate::dispatch::{method_index_for_op, resolve_binop};
use crate::env::{DispatchInfo, TypeEnv};
use crate::errors::{SemaError, SemaErrorKind};
use crate::types::{SemaTypeId, Ty};
use crate::unify::unify;

/// Bidirectional type checker.
///
/// Walks the resolved AST, synthesizing types bottom-up and checking
/// against expected types top-down. Records inferred types, dispatch
/// decisions, and errors.
pub struct Checker<'a> {
    db: &'a Db,
    resolution: &'a ResolutionMap,
    env: TypeEnv,
    errors: Vec<SemaError>,
}

impl<'a> Checker<'a> {
    /// Creates a new checker with a seeded type environment.
    #[must_use]
    pub fn new(db: &'a Db, resolution: &'a ResolutionMap) -> Self {
        let mut env = TypeEnv::new();
        env.seed_builtins();
        Self {
            db,
            resolution,
            env,
            errors: Vec::new(),
        }
    }

    /// Type-checks all top-level expressions in the module.
    pub fn check_module(&mut self) {
        let root = self.db.ast.root.clone();
        for &expr_id in &root {
            let _ = self.synth(expr_id);
        }
    }

    /// Consumes the checker, returning the type environment and errors.
    #[must_use]
    pub fn finish(self) -> (TypeEnv, Vec<SemaError>) {
        (self.env, self.errors)
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
                ret_ty: _,
                body,
            } => self.synth_lambda(params, body),
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
            ExprKind::Need(e) => self.synth(e),
            ExprKind::Postfix { expr, .. }
            | ExprKind::Index { expr, .. }
            | ExprKind::TypeOp { expr, .. } => self.synth(expr),
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
            ExprKind::Import { .. }
            | ExprKind::ForeignImport(_)
            | ExprKind::Quote(_)
            | ExprKind::Splice(_)
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
                if args.len() == 1 {
                    let _ = self.check(args[0], param);
                } else {
                    for &arg in args {
                        let _ = self.synth(arg);
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
        if let Some(value) = binding.value {
            let _ = self.synth(value);
        }
        self.env.intern(Ty::Unit)
    }

    fn synth_lambda(&mut self, params: &[Param], body: ExprId) -> SemaTypeId {
        let param_ty = if params.len() == 1 {
            self.env.fresh_var()
        } else {
            let param_tys: Vec<SemaTypeId> = params.iter().map(|_| self.env.fresh_var()).collect();
            self.env.intern(Ty::Tuple(param_tys))
        };

        let body_ty = self.synth(body);
        self.env.intern(Ty::Arrow {
            param: param_ty,
            ret: body_ty,
        })
    }

    fn synth_match(&mut self, scrutinee: ExprId, arms: &[MatchArm], span: Span) -> SemaTypeId {
        let _ = self.synth(scrutinee);

        if arms.is_empty() {
            return self.env.intern(Ty::Never);
        }

        let mut result_ty = self.synth(arms[0].body);

        for arm in &arms[1..] {
            let arm_ty = self.synth(arm.body);
            match unify(&mut self.env, result_ty, arm_ty, span) {
                Ok(ty) => result_ty = ty,
                Err(e) => self.errors.push(e),
            }
        }

        result_ty
    }

    fn synth_seq(&mut self, stmts: &[ExprId]) -> SemaTypeId {
        if stmts.is_empty() {
            return self.env.intern(Ty::Unit);
        }

        let mut last_ty = self.env.intern(Ty::Unit);
        for &stmt in stmts {
            last_ty = self.synth(stmt);
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
            _ => self.env.intern(Ty::Any),
        }
    }

    fn synth_assign(&mut self, lhs: ExprId, rhs: ExprId, span: Span) -> SemaTypeId {
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
