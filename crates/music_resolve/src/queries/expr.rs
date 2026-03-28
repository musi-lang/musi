use super::*;

impl ResolveDb {
    pub(super) fn resolve_expr(&mut self, expr_id: ExprId, scope: ScopeId) {
        let kind = self.db.ast.exprs.get(expr_id).kind.clone();
        match kind {
            ExprKind::Var(ident) => match self.resolution.scopes.resolve(scope, ident.name) {
                Some(def_id) => {
                    let _ = self.resolution.expr_res.insert(expr_id, def_id);
                    let def_scope = self.resolution.defs.get(def_id).scope;
                    if self
                        .resolution
                        .scopes
                        .crosses_lambda_boundary(def_scope, scope)
                        && let Some(lambda_id) = self.current_lambda
                    {
                        let captures = self.resolution.captures.entry(lambda_id).or_default();
                        if !captures.contains(&ident.name) {
                            captures.push(ident.name);
                        }
                    }
                }
                None => {
                    self.errors.push(ResolveError {
                        kind: crate::errors::ResolveErrorKind::UndefinedName(ident.name),
                        span: ident.span,
                    });
                }
            },
            ExprKind::Lit(_) | ExprKind::Import { .. } | ExprKind::ForeignImport(_) => {}
            ExprKind::TupleLit(elems)
            | ExprKind::ArrayLit(elems)
            | ExprKind::VariantLit(_, elems) => {
                for e in elems {
                    self.resolve_expr(e, scope);
                }
            }
            ExprKind::Let(binding) => self.resolve_let(&binding, scope),
            ExprKind::Seq(stmts) => self.resolve_seq(stmts, scope),
            ExprKind::Lambda {
                params,
                body,
                ret_ty,
            } => {
                self.resolve_lambda(expr_id, params, ret_ty, body, scope);
            }
            ExprKind::Case(ref data) => self.resolve_case(data.scrutinee, &data.arms, scope),
            ExprKind::Comprehension(ref data) => {
                self.resolve_comprehension(data.expr, &data.clauses, scope);
            }
            ExprKind::BinOp(_, lhs, rhs) | ExprKind::Assign(lhs, rhs) => {
                self.resolve_expr(lhs, scope);
                self.resolve_expr(rhs, scope);
            }
            ExprKind::UnaryOp(_, operand)
            | ExprKind::Perform(operand)
            | ExprKind::Postfix { expr: operand, .. }
            | ExprKind::Access { expr: operand, .. } => {
                self.resolve_expr(operand, scope);
            }
            ExprKind::App(callee, args) => {
                self.resolve_expr(callee, scope);
                for arg in args {
                    self.resolve_expr(arg, scope);
                }
            }
            ExprKind::Return(opt) | ExprKind::Resume(opt) => {
                if let Some(e) = opt {
                    self.resolve_expr(e, scope);
                }
            }
            ExprKind::Index { expr, indices, .. } => {
                self.resolve_expr(expr, scope);
                for i in indices {
                    self.resolve_expr(i, scope);
                }
            }
            ExprKind::TypeOp { expr, ty, .. } => {
                self.resolve_expr(expr, scope);
                self.resolve_ty(ty, scope);
            }
            ExprKind::Branch {
                cond,
                then_br,
                else_br,
            } => {
                self.resolve_expr(cond, scope);
                self.resolve_expr(then_br, scope);
                self.resolve_expr(else_br, scope);
            }
            _ => self.resolve_expr_compound(kind, scope),
        }
    }

    pub(super) fn resolve_expr_compound(&mut self, kind: ExprKind, scope: ScopeId) {
        match kind {
            ExprKind::Piecewise(arms) => {
                for arm in arms {
                    self.resolve_expr(arm.value, scope);
                    if let PwGuard::Expr(e) = arm.guard {
                        self.resolve_expr(e, scope);
                    }
                }
            }
            ExprKind::MatrixLit(rows) => {
                for row in rows {
                    for e in row {
                        self.resolve_expr(e, scope);
                    }
                }
            }
            ExprKind::RecordLit(fields) => self.resolve_record_fields(&fields, scope),
            ExprKind::RecordUpdate { base, fields } => {
                self.resolve_expr(base, scope);
                self.resolve_record_fields(&fields, scope);
            }
            ExprKind::FStrLit(parts) => {
                for part in parts {
                    if let FStrPart::Expr(e) = part {
                        self.resolve_expr(e, scope);
                    }
                }
            }
            ExprKind::Handle(ref data) => {
                for clause in &data.clauses {
                    let handler_scope = self
                        .resolution
                        .scopes
                        .push(ScopeKind::Function, Some(scope));
                    match clause {
                        HandlerClause::Return { binder, body } => {
                            let _ = self.define_value(binder, handler_scope);
                            self.resolve_expr(*body, handler_scope);
                        }
                        HandlerClause::Op {
                            args, cont, body, ..
                        } => {
                            for arg in args {
                                let _ = self.define_value(arg, handler_scope);
                            }
                            let _ = self.define_value(cont, handler_scope);
                            self.resolve_expr(*body, handler_scope);
                        }
                    }
                }
                self.resolve_expr(data.body, scope);
            }
            ExprKind::Quote(qk) => match qk {
                QuoteKind::Expr(e) => self.resolve_expr(e, scope),
                QuoteKind::Block(es) => {
                    for e in es {
                        self.resolve_expr(e, scope);
                    }
                }
            },
            ExprKind::Splice(sk) => match sk {
                SpliceKind::Expr(e) => self.resolve_expr(e, scope),
                SpliceKind::Array(es) => {
                    for e in es {
                        self.resolve_expr(e, scope);
                    }
                }
                SpliceKind::Ident(_) => {}
            },
            ExprKind::DataDef(ref body) => match body.as_ref() {
                DataBody::Product(fields) => {
                    for field in fields {
                        self.resolve_ty(field.ty, scope);
                        if let Some(default) = field.default {
                            self.resolve_expr(default, scope);
                        }
                    }
                }
                DataBody::Sum(variants) => {
                    for variant in variants {
                        if let Some(payload) = variant.payload {
                            self.resolve_ty(payload, scope);
                        }
                        if let Some(default) = variant.default {
                            self.resolve_expr(default, scope);
                        }
                    }
                }
            },
            ExprKind::EffectDef(ref members) => {
                self.resolve_member_bodies(members, scope);
            }
            ExprKind::ClassDef(ref data) => {
                self.resolve_member_bodies(&data.members, scope);
            }
            ExprKind::InstanceDef(inst) => {
                if let InstanceBody::Methods(ref members) = inst.body {
                    self.resolve_member_bodies(members, scope);
                }
            }
            _ => {}
        }
    }

    pub(super) fn resolve_seq(&mut self, stmts: ExprList, scope: ScopeId) {
        let seq_scope = self.resolution.scopes.push(ScopeKind::Block, Some(scope));
        for stmt in stmts {
            let stmt_kind = self.db.ast.exprs.get(stmt).kind.clone();
            if let ExprKind::Let(binding) = stmt_kind {
                self.resolve_let(&binding, seq_scope);
            } else {
                self.resolve_expr(stmt, seq_scope);
            }
        }
    }

    pub(super) fn resolve_lambda(
        &mut self,
        expr_id: ExprId,
        params: ParamList,
        ret_ty: Option<TyId>,
        body: ExprId,
        scope: ScopeId,
    ) {
        let lambda_scope = self.resolution.scopes.push(ScopeKind::Lambda, Some(scope));
        self.bind_params(&params, lambda_scope);
        for param in params {
            if let Some(ty) = param.ty {
                self.resolve_ty(ty, scope);
            }
        }
        if let Some(rt) = ret_ty {
            self.resolve_ty(rt, scope);
        }
        let saved_lambda = self.current_lambda;
        self.current_lambda = Some(expr_id);
        self.resolve_expr(body, lambda_scope);
        self.current_lambda = saved_lambda;
    }

    pub(super) fn resolve_case(&mut self, scrutinee: ExprId, arms: &[CaseArm], scope: ScopeId) {
        self.resolve_expr(scrutinee, scope);
        for arm in arms {
            let arm_scope = self.resolution.scopes.push(ScopeKind::CaseArm, Some(scope));
            self.resolve_pat(arm.pat, arm_scope);
            if let Some(guard) = arm.guard {
                self.resolve_expr(guard, arm_scope);
            }
            self.resolve_expr(arm.body, arm_scope);
        }
    }

    pub(super) fn resolve_comprehension(
        &mut self,
        expr: ExprId,
        clauses: &[CompClause],
        scope: ScopeId,
    ) {
        let comp_scope = self
            .resolution
            .scopes
            .push(ScopeKind::Comprehension, Some(scope));
        for clause in clauses {
            match *clause {
                CompClause::Generator { pat, iter } => {
                    self.resolve_expr(iter, comp_scope);
                    self.resolve_pat(pat, comp_scope);
                }
                CompClause::Filter(e) => {
                    self.resolve_expr(e, comp_scope);
                }
            }
        }
        self.resolve_expr(expr, comp_scope);
    }

    pub(super) fn resolve_record_fields(&mut self, fields: &[RecordField], scope: ScopeId) {
        for field in fields {
            match field {
                RecordField::Named { value, .. } => {
                    if let Some(v) = value {
                        self.resolve_expr(*v, scope);
                    }
                }
                RecordField::Spread(e) => {
                    self.resolve_expr(*e, scope);
                }
            }
        }
    }

    pub(super) fn resolve_member_bodies(&mut self, members: &[MemberDecl], scope: ScopeId) {
        for member in members {
            match member {
                MemberDecl::Fn(decl) => {
                    if let Some(body) = decl.body {
                        self.resolve_expr(body, scope);
                    }
                }
                MemberDecl::Law(law) => {
                    self.resolve_expr(law.body, scope);
                }
            }
        }
    }
}
