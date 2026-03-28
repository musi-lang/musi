use music_ast::common::MemberName;
use music_ast::expr::{
    BinOp, ExprKind, FieldTarget, IndexKind, PiecewiseArm, PwGuard, RecordField, SpliceKind,
    UnaryOp,
};
use music_owned::prelude::PRELUDE_MODULE_NAME;
use music_owned::types::BuiltinType;
use music_resolve::DefId;
use music_resolve::def::DefKind;
use music_shared::{Ident, Literal, Span, Symbol};

use super::{QuickExpr, SemaDb};
use crate::dispatch::{method_index_for_op, resolve_binop};
use crate::env::DispatchInfo;
use crate::errors::{SemaError, SemaErrorKind};
use crate::types::{SemaTypeId, SemaTypeList, Ty};
use crate::unify::unify;
use music_ast::ExprId;
use music_il::opcode::Opcode;

impl SemaDb {
    /// Synthesizes a type for `expr_id` bottom-up, caching the result.
    pub(super) fn synth(&mut self, expr_id: ExprId) -> SemaTypeId {
        if let Some(&ty) = self.env.type_map.get(&expr_id) {
            return ty;
        }

        let spanned = self.db.ast.exprs.get(expr_id);
        let span = spanned.span;
        let quick = match &spanned.kind {
            ExprKind::Var(ident) => QuickExpr::Var(expr_id, *ident),
            ExprKind::Lit(lit) => QuickExpr::Lit(lit.clone()),
            ExprKind::BinOp(op, lhs, rhs) => QuickExpr::BinOp(*op, *lhs, *rhs),
            ExprKind::UnaryOp(op, operand) => QuickExpr::UnaryOp(*op, *operand),
            ExprKind::Assign(lhs, rhs) => QuickExpr::Assign(*lhs, *rhs),
            ExprKind::Branch {
                cond,
                then_br,
                else_br,
            } => QuickExpr::Branch(*cond, *then_br, *else_br),
            ExprKind::Return(val) => QuickExpr::Return(*val),
            ExprKind::Perform(e) => QuickExpr::Perform(*e),
            ExprKind::Postfix { expr, .. } | ExprKind::TypeOp { expr, .. } => {
                QuickExpr::Passthrough(*expr)
            }
            ExprKind::FStrLit(_) => QuickExpr::FStr,
            ExprKind::ClassDef(_) => QuickExpr::ClassDef,
            ExprKind::RecordUpdate { base, .. } => QuickExpr::Passthrough(*base),
            _ => QuickExpr::Other,
        };

        let ty = match quick {
            QuickExpr::Var(var_expr_id, ident) => self.synth_var(var_expr_id, &ident),
            QuickExpr::Lit(ref lit) => self.synth_literal(lit),
            QuickExpr::BinOp(op, lhs, rhs) => self.synth_binop(op, lhs, rhs, span),
            QuickExpr::UnaryOp(op, operand) => self.synth_unary(op, operand, expr_id),
            QuickExpr::Assign(lhs, rhs) => self.synth_assign(lhs, rhs, span),
            QuickExpr::Branch(cond, then_br, else_br) => {
                self.synth_branch(cond, then_br, else_br, span)
            }
            QuickExpr::Return(val) => {
                if let Some(v) = val {
                    let _ = self.synth(v);
                }
                self.env.intern(Ty::Empty)
            }
            QuickExpr::Perform(e) => self.synth_perform(e, span, expr_id),
            QuickExpr::Passthrough(expr) => self.synth(expr),
            QuickExpr::FStr => self.env.builtin(BuiltinType::String),
            QuickExpr::ClassDef => self.env.intern(Ty::Builtin(BuiltinType::Type)),
            QuickExpr::Other => self.synth_full_clone(expr_id, span),
        };

        let _ = self.env.type_map.insert(expr_id, ty);
        ty
    }

    /// Handles the remaining `ExprKind` variants that require a full clone.
    pub(super) fn synth_full_clone(&mut self, expr_id: ExprId, span: Span) -> SemaTypeId {
        let kind = self.db.ast.exprs.get(expr_id).kind.clone();
        match kind {
            ExprKind::App(callee, ref args) => self.synth_app(callee, args, span),
            ExprKind::Let(ref binding) => self.synth_let(binding),
            ExprKind::Lambda {
                ref params,
                ret_ty,
                body,
            } => self.synth_lambda(params, ret_ty, body),
            ExprKind::Case(ref data) => self.synth_case(data.scrutinee, &data.arms, span),
            ExprKind::Seq(ref stmts) => self.synth_seq(stmts),
            ExprKind::TupleLit(ref elems) => self.synth_tuple(elems),
            ExprKind::ArrayLit(ref elems) => self.synth_array(elems, span),
            ExprKind::RecordLit(ref fields) => self.synth_record_lit(fields),
            ExprKind::VariantLit(ref tag, ref args) => self.synth_variant_lit(tag, args, expr_id),
            ExprKind::Access {
                expr, ref field, ..
            } => self.synth_access(expr, field, span),
            ExprKind::Resume(val) => self.synth_resume(val, span),
            ExprKind::Index {
                expr,
                ref indices,
                kind,
            } => self.synth_index(expr, indices, kind, span),
            ExprKind::DataDef(ref body) => {
                self.register_data_variants(body);
                self.env.intern(Ty::Builtin(BuiltinType::Type))
            }
            ExprKind::EffectDef(ref members) => self.synth_effect_def(members),
            ExprKind::InstanceDef(ref inst) => self.synth_instance_def(expr_id, inst, span),
            ExprKind::Handle(ref data) => {
                self.synth_handle(&data.effect, &data.clauses, data.body, span, expr_id)
            }
            ExprKind::Comprehension(ref data) => {
                let elem_ty = self.synth(data.expr);
                self.env.intern(Ty::Array(elem_ty))
            }
            ExprKind::Quote(_) => self.synth_quote(),
            ExprKind::Splice(ref sk) => self.synth_splice(sk, span),
            ExprKind::Var(_)
            | ExprKind::Lit(_)
            | ExprKind::BinOp(..)
            | ExprKind::UnaryOp(..)
            | ExprKind::Assign(..)
            | ExprKind::Branch { .. }
            | ExprKind::Return(_)
            | ExprKind::Perform(_)
            | ExprKind::Postfix { .. }
            | ExprKind::TypeOp { .. }
            | ExprKind::FStrLit(_)
            | ExprKind::ClassDef(_)
            | ExprKind::RecordUpdate { .. }
            | ExprKind::Import { .. }
            | ExprKind::ForeignImport(_)
            | ExprKind::MatrixLit(_) => self.env.intern(Ty::Any),
            ExprKind::Piecewise(ref arms) => self.synth_piecewise(arms, span),
        }
    }

    /// Checks `expr_id` against `expected`, unifying and reporting mismatches.
    pub(super) fn check(&mut self, expr_id: ExprId, expected: SemaTypeId) -> SemaTypeId {
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

    pub(super) fn synth_piecewise(&mut self, arms: &[PiecewiseArm], span: Span) -> SemaTypeId {
        let result_ty = self.env.fresh_var();
        let bool_ty = self.env.builtin(BuiltinType::Bool);

        for arm in arms {
            if let PwGuard::Expr(guard) = arm.guard {
                let guard_ty = self.synth(guard);
                if let Err(error) = unify(&mut self.env, guard_ty, bool_ty, span) {
                    self.errors.push(error);
                }
            }

            let value_ty = self.synth(arm.value);
            if let Err(error) = unify(&mut self.env, value_ty, result_ty, span) {
                self.errors.push(error);
            }
        }

        result_ty
    }

    pub(super) fn synth_literal(&self, lit: &Literal) -> SemaTypeId {
        match lit {
            Literal::Int(_) => self.env.builtin(BuiltinType::Int),
            Literal::Float(_) => self.env.builtin(BuiltinType::Float),
            Literal::Str(_) => self.env.builtin(BuiltinType::String),
            Literal::Rune(_) => self.env.builtin(BuiltinType::Rune),
        }
    }

    pub(super) fn synth_var(&mut self, expr_id: ExprId, ident: &Ident) -> SemaTypeId {
        if let Some(&def_id) = self.resolution.expr_res.get(&expr_id) {
            let _inserted = self.used_defs.insert(def_id);
            if self.depth > 0 && self.mutable_defs.contains(&def_id) {
                let _inserted = self.env.captured_mutables.insert(def_id);
                let _inserted = self.env.captured_mutable_names.insert(ident.name);
            }
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

    pub(super) fn find_let_value_type(&self, name: Symbol) -> Option<SemaTypeId> {
        self.let_types.get(&name).copied()
    }

    pub(super) fn find_def_for_name(&self, name: Symbol) -> Option<DefId> {
        self.name_to_def.get(&name).copied()
    }

    pub(super) fn builtin_method_opcode(&self, name: Symbol) -> Option<Opcode> {
        let def_id = self.find_def_for_name(name)?;
        let def = self.resolution.defs.get(def_id);
        if !matches!(def.kind, DefKind::Method)
            || def.module_name.as_deref() != Some(PRELUDE_MODULE_NAME)
        {
            return None;
        }
        self.intrinsic_methods.get(&name).copied()
    }

    pub(super) fn synth_app(&mut self, callee: ExprId, args: &[ExprId], span: Span) -> SemaTypeId {
        let callee_kind = self.db.ast.exprs.get(callee).kind.clone();
        if let ExprKind::Var(ref ident) = callee_kind {
            if let Some(opcode) = self.builtin_method_opcode(ident.name) {
                let _ = self
                    .env
                    .dispatch
                    .insert(callee, DispatchInfo::Static { opcode });
            }
        }

        let callee_ty = self.synth(callee);
        let resolved = self.env.resolve_var(callee_ty);
        let ty = self.env.types.get(resolved).clone();

        let result_ty = match ty {
            Ty::Arrow { param, ret } | Ty::EffectArrow { param, ret, .. } => {
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
            Ty::EffectOp { ret, .. } => {
                for &arg in args {
                    let _ = self.synth(arg);
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
        };

        self.check_constraints_at_call(callee, args, span);

        result_ty
    }

    pub(super) fn synth_binop(
        &mut self,
        op: BinOp,
        lhs: ExprId,
        rhs: ExprId,
        span: Span,
    ) -> SemaTypeId {
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
        if matches!(op, BinOp::Shl | BinOp::Shr) {
            let int_ty = self.env.builtin(BuiltinType::Int);
            if let Err(e) = unify(&mut self.env, rhs_ty, int_ty, span) {
                self.errors.push(e);
            }
        }

        resolution.result_ty
    }

    pub(super) fn synth_unary(
        &mut self,
        op: UnaryOp,
        operand: ExprId,
        expr_id: ExprId,
    ) -> SemaTypeId {
        let operand_ty = self.synth(operand);
        match op {
            UnaryOp::Neg => {
                let resolved = self.env.resolve_var(operand_ty);
                let ty = self.env.types.get(resolved).clone();
                if let Ty::Builtin(bt) = ty {
                    if matches!(
                        bt,
                        BuiltinType::Float | BuiltinType::Float32 | BuiltinType::Float64
                    ) {
                        let _ = self.env.dispatch.insert(
                            expr_id,
                            DispatchInfo::Static {
                                opcode: Opcode::FNeg,
                            },
                        );
                    }
                }
                operand_ty
            }
            UnaryOp::Spread => operand_ty,
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

    pub(super) fn synth_branch(
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

    pub(super) fn synth_seq(&mut self, stmts: &[ExprId]) -> SemaTypeId {
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
            if matches!(self.env.types.get(resolved), Ty::Empty) {
                diverged = true;
            }
        }
        last_ty
    }

    pub(super) fn synth_tuple(&mut self, elems: &[ExprId]) -> SemaTypeId {
        let elem_tys: SemaTypeList = elems.iter().map(|&e| self.synth(e)).collect();
        self.env.intern(Ty::Tuple(elem_tys))
    }

    pub(super) fn synth_array(&mut self, elems: &[ExprId], span: Span) -> SemaTypeId {
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

    pub(super) fn synth_record_lit(&mut self, fields: &[RecordField]) -> SemaTypeId {
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

    pub(super) fn synth_access(
        &mut self,
        expr: ExprId,
        field: &FieldTarget,
        span: Span,
    ) -> SemaTypeId {
        let base_ty = self.synth(expr);
        let resolved = self.env.resolve_var(base_ty);
        let ty = self.env.types.get(resolved).clone();

        match (&ty, field) {
            (Ty::Array(_), FieldTarget::Name(ident))
                if self.db.interner.resolve(ident.name) == "len" =>
            {
                self.env.builtin(BuiltinType::Int)
            }
            (Ty::Builtin(BuiltinType::String), FieldTarget::Name(ident))
                if self.db.interner.resolve(ident.name) == "len" =>
            {
                self.env.builtin(BuiltinType::Int)
            }
            (Ty::Effect(effect_name), FieldTarget::Name(ident)) => {
                let ret_ty = self.env.builtin(BuiltinType::Unit);
                let effect_op = self
                    .env
                    .effect_op_info(*effect_name, ident.name)
                    .cloned()
                    .map(|op| (op.name, op.ret_ty))
                    .unwrap_or_else(|| (ident.name, ret_ty));
                self.env.intern(Ty::EffectOp {
                    effect: *effect_name,
                    op: effect_op.0,
                    ret: effect_op.1,
                })
            }
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

    pub(super) fn synth_quote(&mut self) -> SemaTypeId {
        let prev = self.in_quote;
        self.in_quote = true;
        let ty = self.env.intern(Ty::Any);
        self.in_quote = prev;
        ty
    }

    pub(super) fn synth_splice(&mut self, sk: &SpliceKind, span: Span) -> SemaTypeId {
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

    pub(super) fn synth_index(
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

    pub(super) fn synth_assign(&mut self, lhs: ExprId, rhs: ExprId, span: Span) -> SemaTypeId {
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

pub(super) const fn member_name_symbol(name: &MemberName) -> Symbol {
    match name {
        MemberName::Ident(ident) | MemberName::Op(ident, _) => ident.name,
    }
}
