//! Pass 2: expression resolution.

use music_ast::expr::{
    Arg, ArrayElem, Expr, FieldKey, HandlerOp, LetFields, MatchArm, Param, PwArm, PwGuard,
    RecDefField, RecField,
};
use music_ast::lit::{FStrPart, Lit};
use music_ast::pat::Pat;
use music_ast::{ExprIdx, NameRefIdx};
use music_shared::{Span, Symbol};

use crate::def::{DefId, DefKind};
use crate::error::SemaError;

use super::{Resolver, binding_def_kind};

impl Resolver<'_> {
    /// Pass 2: resolve all name references in an expression.
    #[allow(clippy::too_many_lines)]
    pub(super) fn resolve_expr(&mut self, expr_idx: ExprIdx) {
        match self.ast.exprs[expr_idx].clone() {
            Expr::Name { name_ref, span } => {
                let nr = self.ast.name_refs[name_ref];
                self.resolve_name(expr_idx, name_ref, nr.name, span);
            }
            Expr::Lit { ref lit, .. } => self.resolve_lit(lit),
            Expr::Error { .. } | Expr::Import { .. } | Expr::Export { .. } => {}
            Expr::Paren { inner, .. } | Expr::Annotated { inner, .. } => self.resolve_expr(inner),
            Expr::Choice { body, .. } => self.resolve_expr_choice(body, None),
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
            Expr::UnaryOp { operand, .. } => {
                self.resolve_expr(operand);
            }
            Expr::Field { object, field, .. } => {
                self.resolve_expr(object);
                if let FieldKey::Name { name, .. } = field {
                    if let Some(&alias_def_id) = self.output.expr_defs.get(&object)
                        && let Some(&import_path) = self.import_alias_defs.get(&alias_def_id)
                        && let Some(names) = self.import_names.get(&import_path)
                        && let Some(&(_, exported_def_id)) = names.iter().find(|(n, _)| *n == name)
                    {
                        let _prev = self.output.expr_defs.insert(expr_idx, exported_def_id);
                        self.defs.get_mut(exported_def_id).use_count += 1;
                    } else if let Some(&obj_def_id) = self.output.expr_defs.get(&object)
                        && let Some(sub_names) = self.sub_module_exports.get(&obj_def_id)
                        && let Some(&(_, exported_def_id)) =
                            sub_names.iter().find(|(n, _)| *n == name)
                    {
                        let _prev = self.output.expr_defs.insert(expr_idx, exported_def_id);
                        self.defs.get_mut(exported_def_id).use_count += 1;
                    }
                }
            }
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
            Expr::RecordDef { fields, .. } => self.resolve_rec_def_fields(&fields),
            Expr::Array { elems, .. } => self.resolve_expr_array(&elems),
            Expr::Piecewise { arms, .. } => self.resolve_expr_piecewise(&arms),
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
            Expr::Class {
                name,
                params,
                constraints,
                members,
                ..
            } => {
                self.resolve_expr_class(name, &params, &constraints, &members);
            }
            Expr::Instance {
                target,
                params,
                constraints,
                members,
                ..
            } => {
                self.resolve_expr_given(target, &params, &constraints, &members);
            }
            Expr::Effect {
                name,
                params,
                ops,
                exported,
                ..
            } => self.resolve_expr_effect(name, &params, &ops, exported),
            Expr::Foreign { decls, .. } => self.resolve_expr_foreign(&decls),
            Expr::TypeCheck { operand, ty, .. } => {
                self.resolve_expr(operand);
                self.resolve_type_expr(ty);
            }
            Expr::Handle {
                effect_ty,
                ops,
                body,
                ..
            } => self.resolve_expr_handle(effect_ty, &ops, body),
            // Type-expression variants: resolve names within them as type references.
            Expr::TypeApp { .. }
            | Expr::FnType { .. }
            | Expr::OptionType { .. }
            | Expr::ProductType { .. }
            | Expr::SumType { .. }
            | Expr::ArrayType { .. }
            | Expr::PiType { .. } => self.resolve_type_expr(expr_idx),
        }
    }

    fn resolve_lit(&mut self, lit: &Lit) {
        if let Lit::FStr { parts, .. } = lit {
            for part in parts {
                if let FStrPart::Interpolated { expr, .. } = part {
                    self.resolve_expr(*expr);
                }
            }
        }
    }

    pub(super) fn resolve_name(
        &mut self,
        expr_idx: ExprIdx,
        name_ref: NameRefIdx,
        name: Symbol,
        span: Span,
    ) {
        if let Some(def_id) = self.scopes.lookup(self.current_scope, name) {
            let _prev = self.output.expr_defs.insert(expr_idx, def_id);
            self.output.name_ref_defs[usize::try_from(name_ref.raw()).unwrap()] = Some(def_id);
            self.defs.get_mut(def_id).use_count += 1;
        } else {
            self.report_undefined(name, span);
        }
    }

    pub(super) fn report_undefined(&mut self, name: Symbol, span: Span) {
        let name_str = self.interner.resolve(name);
        let _d = self.diags.report(
            &SemaError::UndefinedName {
                name: Box::from(name_str),
            },
            span,
            self.file_id,
        );
    }

    fn resolve_rec_def_fields(&mut self, fields: &[RecDefField]) {
        for f in fields {
            self.resolve_type_expr(f.ty);
        }
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
        let is_lambda_bind = !is_fn_pat
            && fields
                .value
                .is_some_and(|v| matches!(&self.ast.exprs[v], Expr::Fn { .. }));

        // For function-like patterns, pre-define the name to enable recursion.
        if is_fn_pat {
            self.define_fn_name(fields.pat, binding_def_kind(fields.kind));
        } else if is_lambda_bind {
            self.define_pat(fields.pat, binding_def_kind(fields.kind));
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
            self.resolve_type_expr(ty);
        }
        if let Some(p) = fn_pat_parent {
            self.current_scope = p;
        }
        if let Some(p) = parent_ty_scope {
            self.current_scope = p;
        }
        if !is_fn_pat && !is_lambda_bind {
            self.define_pat(fields.pat, binding_def_kind(fields.kind));
        }
    }

    fn resolve_expr_call(&mut self, callee: ExprIdx, args: &[Arg]) {
        self.resolve_expr(callee);
        for arg in args {
            match arg {
                Arg::Pos { expr, .. } | Arg::Spread { expr, .. } => self.resolve_expr(*expr),
            }
        }
    }

    fn resolve_expr_let(&mut self, fields: &LetFields, body: Option<ExprIdx>) {
        let is_fn_pat = matches!(&self.ast.pats[fields.pat], Pat::Variant { .. });
        let is_lambda_bind = !is_fn_pat
            && fields
                .value
                .is_some_and(|v| matches!(&self.ast.exprs[v], Expr::Fn { .. }));

        let parent_ty_scope = if fields.params.is_empty() {
            None
        } else {
            Some(self.enter_ty_param_scope(&fields.params, &fields.constraints))
        };

        // For let-in expressions, pre-define function names for recursion.
        // Top-level `let` (body: None) is pre-defined in collect_top_level.
        if is_fn_pat && body.is_some() {
            self.define_fn_name(fields.pat, binding_def_kind(fields.kind));
        } else if is_lambda_bind && body.is_some() {
            self.define_pat(fields.pat, binding_def_kind(fields.kind));
        }

        let fn_pat_parent = self.enter_fn_pat_scope(fields.pat);

        if let Some(v) = fields.value {
            self.resolve_expr(v);
        }
        if let Some(ty) = fields.ty {
            self.resolve_type_expr(ty);
        }

        if let Some(p) = fn_pat_parent {
            self.current_scope = p;
        }

        if let Some(body) = body {
            let parent = self.current_scope;
            self.current_scope = self.scopes.push_child(parent);
            if !is_fn_pat && !is_lambda_bind {
                self.define_pat(fields.pat, binding_def_kind(fields.kind));
            }
            self.resolve_expr(body);
            self.current_scope = parent;
        }
        // Top-level `let` (body: None) - names already defined in collect_top_level.

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
            if matches!(&self.ast.exprs[v], Expr::Choice { .. }) {
                let pat_span = match &self.ast.pats[fields.pat] {
                    Pat::Variant { span, .. } | Pat::Bind { span, .. } => Some(*span),
                    _ => None,
                };
                let parent_def = pat_span.and_then(|s| self.output.pat_defs.get(&s).copied());
                if let Expr::Choice { body, .. } = &self.ast.exprs[v] {
                    self.resolve_expr_choice(*body, parent_def);
                }
            } else {
                self.resolve_expr(v);
            }
        }
        if let Some(ty) = fields.ty {
            self.resolve_type_expr(ty);
        }

        if let Some(p) = fn_pat_parent {
            self.current_scope = p;
        }

        if let Some(p) = parent_ty_scope {
            self.current_scope = p;
        }
    }

    fn resolve_expr_fn(&mut self, params: &[Param], ret_ty: Option<ExprIdx>, body: ExprIdx) {
        let parent = self.current_scope;
        self.current_scope = self.scopes.push_child(parent);
        for param in params {
            let id = self
                .defs
                .alloc(param.name, DefKind::Param, param.span, self.file_id);
            self.defs.get_mut(id).param_mode = Some(param.mode);
            self.define_in_scope(param.name, id, param.span);
            let _inserted = self.output.pat_defs.insert(param.span, id);
            if let Some(ty) = param.ty {
                self.resolve_type_expr(ty);
            }
        }
        if let Some(ret) = ret_ty {
            self.resolve_type_expr(ret);
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

    fn resolve_expr_choice(&mut self, body: ExprIdx, choice_parent: Option<DefId>) {
        let parent = self.current_scope;
        self.current_scope = self.scopes.push_child(parent);

        match self.ast.exprs[body].clone() {
            Expr::SumType { variants, .. } => {
                for &variant_expr in &variants {
                    if let Expr::Name { name_ref, span } = &self.ast.exprs[variant_expr] {
                        let name = self.ast.name_refs[*name_ref].name;
                        let id = self.defs.alloc(name, DefKind::Variant, *span, self.file_id);
                        if let Some(p) = choice_parent {
                            self.defs.get_mut(id).parent = Some(p);
                        }
                        self.define_in_scope(name, id, *span);
                    } else if let Expr::TypeApp { callee, .. } = &self.ast.exprs[variant_expr] {
                        if let Expr::Name { name_ref, span } = &self.ast.exprs[*callee] {
                            let name = self.ast.name_refs[*name_ref].name;
                            let id = self.defs.alloc(name, DefKind::Variant, *span, self.file_id);
                            if let Some(p) = choice_parent {
                                self.defs.get_mut(id).parent = Some(p);
                            }
                            self.define_in_scope(name, id, *span);
                        }
                    }
                }
            }
            Expr::Name { name_ref, span } => {
                let name = self.ast.name_refs[name_ref].name;
                let id = self.defs.alloc(name, DefKind::Variant, span, self.file_id);
                if let Some(p) = choice_parent {
                    self.defs.get_mut(id).parent = Some(p);
                }
                self.define_in_scope(name, id, span);
            }
            Expr::TypeApp { callee, .. } => {
                if let Expr::Name { name_ref, span } = &self.ast.exprs[callee] {
                    let name = self.ast.name_refs[*name_ref].name;
                    let id = self.defs.alloc(name, DefKind::Variant, *span, self.file_id);
                    if let Some(p) = choice_parent {
                        self.defs.get_mut(id).parent = Some(p);
                    }
                    self.define_in_scope(name, id, *span);
                }
            }
            _ => {}
        }

        self.resolve_type_expr(body);
        self.current_scope = parent;
    }

    fn resolve_expr_array(&mut self, elems: &[ArrayElem]) {
        for elem in elems {
            match elem {
                ArrayElem::Elem { expr, .. } | ArrayElem::Spread { expr, .. } => {
                    self.resolve_expr(*expr);
                }
            }
        }
    }

    fn resolve_expr_piecewise(&mut self, arms: &[PwArm]) {
        for arm in arms {
            if let PwGuard::When { expr, .. } = arm.guard {
                self.resolve_expr(expr);
            }
            self.resolve_expr(arm.result);
        }
    }

    fn resolve_expr_handle(&mut self, effect_ty: ExprIdx, ops: &[HandlerOp], body: ExprIdx) {
        self.resolve_type_expr(effect_ty);
        for op in ops {
            let parent = self.current_scope;
            self.current_scope = self.scopes.push_child(parent);
            for param in &op.params {
                let id = self
                    .defs
                    .alloc(param.name, DefKind::Param, param.span, self.file_id);
                self.define_in_scope(param.name, id, param.span);
                let _inserted = self.output.pat_defs.insert(param.span, id);
                if let Some(ty) = param.ty {
                    self.resolve_type_expr(ty);
                }
            }
            self.resolve_expr(op.body);
            self.current_scope = parent;
        }
        self.resolve_expr(body);
    }
}
