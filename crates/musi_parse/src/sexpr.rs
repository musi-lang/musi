//! Deterministic S-expression printer for the Musi AST.
//!
//! Output is diff-friendly and intended for snapshot tests.
//! Spans are omitted so that output does not depend on source positions.

use musi_shared::{Idx, Interner, Symbol};

use crate::ast::{
    ArrayItem, Attr, AttrArg, BinOp, BindKind, ChoiceVariant, ClassMember, Cond, Constraint,
    ElifBranch, ExportItem, Expr, FieldInit, ImportClause, LitValue, MatchArm, Modifier, Param,
    ParsedModule, Pat, PatField, PatSuffix, PostfixOp, PrefixOp, RecField, Ty, TyParam,
    VariantPayload,
};

/// Renders `module` as a multi-line S-expression string.
#[must_use]
pub fn dump(module: &ParsedModule, interner: &Interner) -> String {
    let mut p = Printer {
        buf: String::new(),
        indent: 0,
        module,
        interner,
    };
    p.print_module();
    p.buf
}

/// Renders a single expression (for tests).
#[must_use]
pub fn dump_expr(idx: Idx<Expr>, module: &ParsedModule, interner: &Interner) -> String {
    let mut p = Printer {
        buf: String::new(),
        indent: 0,
        module,
        interner,
    };
    p.print_expr(idx);
    p.buf
}

struct FnDefView<'a> {
    attrs: &'a [Attr],
    modifiers: &'a [Modifier],
    name: Symbol,
    ty_params: &'a [TyParam],
    params: &'a [Param],
    ret_ty: Option<&'a Ty>,
    where_clause: &'a [Constraint],
    body: Option<Idx<Expr>>,
}

struct Printer<'a> {
    buf: String,
    indent: usize,
    module: &'a ParsedModule,
    interner: &'a Interner,
}

impl<'a> Printer<'a> {
    fn write(&mut self, s: &str) {
        self.buf.push_str(s);
    }

    fn write_char(&mut self, c: char) {
        self.buf.push(c);
    }

    fn newline_indent(&mut self) {
        self.buf.push('\n');
        for _ in 0..self.indent {
            self.buf.push(' ');
        }
    }

    fn sym(&self, s: Symbol) -> &'a str {
        self.interner.resolve(s)
    }

    fn space_separated_exprs(&mut self, exprs: &[Idx<Expr>]) {
        for (i, &e) in exprs.iter().enumerate() {
            if i > 0 {
                self.write_char(' ');
            }
            self.print_expr(e);
        }
    }

    fn print_module(&mut self) {
        if self.module.items.is_empty() {
            self.write("(module)");
            return;
        }
        self.write("(module");
        self.indent += 2;
        for &item in &self.module.items {
            self.newline_indent();
            self.print_expr(item);
        }
        self.indent -= 2;
        self.write_char(')');
    }

    fn print_expr(&mut self, idx: Idx<Expr>) {
        let expr = self.module.ctx.exprs.get(idx).clone();
        match expr {
            Expr::Lit { ref value, .. } => self.print_lit(value),
            Expr::Ident { name, .. } => {
                self.write("(ident ");
                self.write(self.sym(name));
                self.write_char(')');
            }
            Expr::Unit { .. } => self.write("unit"),
            Expr::Paren { inner, .. } => {
                self.write("(paren ");
                self.print_expr(inner);
                self.write_char(')');
            }
            Expr::Tuple { ref elements, .. } => {
                self.write("(tuple [");
                self.space_separated_exprs(elements);
                self.write("])");
            }
            Expr::Block {
                ref stmts, tail, ..
            } => self.print_block(stmts, tail),
            Expr::Array { ref items, .. } => self.print_array(items),
            Expr::AnonRec { ref fields, .. } => self.print_anon_rec(fields),
            Expr::Binary { op, lhs, rhs, .. } => self.print_binary(op, lhs, rhs),
            Expr::DotPrefix { name, ref args, .. } => {
                self.write("(dot-prefix ");
                self.write(self.sym(name));
                if !args.is_empty() {
                    self.write(" [");
                    self.space_separated_exprs(args);
                    self.write_char(']');
                }
                self.write_char(')');
            }
            Expr::Prefix { op, operand, .. } => self.print_prefix(op, operand),
            Expr::Assign { target, value, .. } => {
                self.write("(assign ");
                self.print_expr(target);
                self.write_char(' ');
                self.print_expr(value);
                self.write_char(')');
            }
            Expr::Postfix { base, ref op, .. } => self.print_postfix(base, op),
            Expr::If {
                ref cond,
                then_body,
                ref elif_chains,
                else_body,
                ..
            } => {
                self.print_if(cond, then_body, elif_chains, else_body);
            }
            Expr::Match {
                scrutinee,
                ref arms,
                ..
            } => self.print_match(scrutinee, arms),
            other => self.print_expr_stmt(&other),
        }
    }

    fn print_expr_stmt(&mut self, expr: &Expr) {
        match expr {
            Expr::While { cond, guard, body, .. } => self.print_while(cond, *guard, *body),
            Expr::Loop { body, post_cond, .. } => self.print_loop(*body, post_cond.as_deref()),
            Expr::For { pat, iter, guard, body, .. } => self.print_for(pat, *iter, *guard, *body),
            Expr::Label { name, body, .. } => {
                self.write("(label ");
                self.write(self.sym(*name));
                self.write_char(' ');
                self.print_expr(*body);
                self.write_char(')');
            }
            Expr::Return { value, .. } => self.print_optional_tag("return", *value),
            Expr::Break { label, value, .. } => self.print_break(*label, *value),
            Expr::Cycle { label, guard, .. } => self.print_cycle(*label, *guard),
            Expr::Defer { body, .. } => {
                self.write("(defer ");
                self.print_expr(*body);
                self.write_char(')');
            }
            Expr::Import { items, path, .. } => self.print_import(items, *path),
            Expr::Export { items, path, .. } => self.print_export(items, *path),
            Expr::Using { name, init, body, .. } => {
                self.write("(using ");
                self.write(self.sym(*name));
                self.write_char(' ');
                self.print_expr(*init);
                self.write_char(' ');
                self.print_expr(*body);
                self.write_char(')');
            }
            other => self.print_expr_decl(other),
        }
    }

    fn print_expr_decl(&mut self, expr: &Expr) {
        match expr {
            Expr::FnDef {
                attrs,
                modifiers,
                name,
                ty_params,
                params,
                ret_ty,
                where_clause,
                body,
                ..
            } => self.print_fn_def(&FnDefView {
                attrs,
                modifiers,
                name: *name,
                ty_params,
                params,
                ret_ty: ret_ty.as_ref(),
                where_clause,
                body: *body,
            }),
            Expr::Lambda {
                attrs,
                ty_params,
                params,
                ret_ty,
                where_clause,
                body,
                ..
            } => self.print_lambda(attrs, ty_params, params, ret_ty.as_ref(), where_clause, *body),
            Expr::ClassDef {
                name,
                ty_params,
                supers,
                members,
                ..
            } => self.print_class_def(*name, ty_params, supers, members),
            Expr::GivenDef {
                class_app,
                constraints,
                members,
                ..
            } => self.print_given_def(class_app, constraints, members),
            Expr::Record {
                attrs,
                modifiers,
                name,
                ty_params,
                fields,
                ..
            } => self.print_record(attrs, modifiers, *name, ty_params, fields),
            Expr::Choice {
                attrs,
                modifiers,
                name,
                ty_params,
                variants,
                ..
            } => self.print_choice(attrs, modifiers, *name, ty_params, variants),
            Expr::Bind {
                attrs,
                modifiers,
                kind,
                pat,
                ty,
                init,
                ..
            } => self.print_bind(attrs, modifiers, *kind, pat, ty.as_ref(), *init),
            Expr::Error { .. } => self.write("error"),
            _ => {}
        }
    }

    fn print_block(&mut self, stmts: &[Idx<Expr>], tail: Option<Idx<Expr>>) {
        self.write("(block");
        self.indent += 2;
        for &s in stmts {
            self.newline_indent();
            self.print_expr(s);
        }
        if let Some(t) = tail {
            self.newline_indent();
            self.print_expr(t);
        }
        self.indent -= 2;
        self.write_char(')');
    }

    fn print_array(&mut self, items: &[ArrayItem]) {
        self.write("(array [");
        for (i, item) in items.iter().enumerate() {
            if i > 0 {
                self.write_char(' ');
            }
            match *item {
                ArrayItem::Single(e) => self.print_expr(e),
                ArrayItem::Spread(e) => {
                    self.write("(spread ");
                    self.print_expr(e);
                    self.write_char(')');
                }
            }
        }
        self.write("])");
    }

    fn print_anon_rec(&mut self, fields: &[FieldInit]) {
        self.write("(anon_rec [");
        for (i, field) in fields.iter().enumerate() {
            if i > 0 {
                self.write_char(' ');
            }
            self.print_rec_lit_field(field);
        }
        self.write("])");
    }

    fn print_binary(&mut self, op: BinOp, lhs: Idx<Expr>, rhs: Idx<Expr>) {
        self.write("(binary ");
        self.write(binop_str(op));
        self.write_char(' ');
        self.print_expr(lhs);
        self.write_char(' ');
        self.print_expr(rhs);
        self.write_char(')');
    }

    fn print_prefix(&mut self, op: PrefixOp, operand: Idx<Expr>) {
        self.write("(prefix ");
        self.write(prefix_str(op));
        self.write_char(' ');
        self.print_expr(operand);
        self.write_char(')');
    }

    fn print_if(
        &mut self,
        cond: &Cond,
        then_body: Idx<Expr>,
        elif_chains: &[ElifBranch],
        else_body: Option<Idx<Expr>>,
    ) {
        self.write("(if ");
        self.print_cond(cond);
        self.write_char(' ');
        self.print_expr(then_body);
        for elif in elif_chains {
            self.print_elif(elif);
        }
        if let Some(eb) = else_body {
            self.write(" (else ");
            self.print_expr(eb);
            self.write_char(')');
        }
        self.write_char(')');
    }

    fn print_match(&mut self, scrutinee: Idx<Expr>, arms: &[MatchArm]) {
        self.write("(match ");
        self.print_expr(scrutinee);
        self.indent += 2;
        for arm in arms {
            self.newline_indent();
            self.print_match_arm(arm);
        }
        self.indent -= 2;
        self.write_char(')');
    }

    fn print_while(&mut self, cond: &Cond, guard: Option<Idx<Expr>>, body: Idx<Expr>) {
        self.write("(while ");
        self.print_cond(cond);
        self.print_optional_guard(guard);
        self.write_char(' ');
        self.print_expr(body);
        self.write_char(')');
    }

    fn print_loop(&mut self, body: Idx<Expr>, post_cond: Option<&Cond>) {
        self.write("(loop ");
        self.print_expr(body);
        if let Some(pc) = post_cond {
            self.write(" (post_while ");
            self.print_cond(pc);
            self.write_char(')');
        }
        self.write_char(')');
    }

    fn print_for(&mut self, pat: &Pat, iter: Idx<Expr>, guard: Option<Idx<Expr>>, body: Idx<Expr>) {
        self.write("(for ");
        self.print_pat(pat);
        self.write_char(' ');
        self.print_expr(iter);
        self.print_optional_guard(guard);
        self.write_char(' ');
        self.print_expr(body);
        self.write_char(')');
    }

    fn print_optional_tag(&mut self, tag: &str, value: Option<Idx<Expr>>) {
        self.write_char('(');
        self.write(tag);
        if let Some(v) = value {
            self.write_char(' ');
            self.print_expr(v);
        }
        self.write_char(')');
    }

    fn print_break(&mut self, label: Option<Symbol>, value: Option<Idx<Expr>>) {
        self.write("(break");
        if let Some(l) = label {
            self.write_char(' ');
            self.write(self.sym(l));
        }
        if let Some(v) = value {
            self.write_char(' ');
            self.print_expr(v);
        }
        self.write_char(')');
    }

    fn print_cycle(&mut self, label: Option<Symbol>, guard: Option<Idx<Expr>>) {
        self.write("(cycle");
        if let Some(l) = label {
            self.write_char(' ');
            self.write(self.sym(l));
        }
        if let Some(g) = guard {
            self.write_char(' ');
            self.print_expr(g);
        }
        self.write_char(')');
    }

    fn print_import(&mut self, items: &ImportClause, path: Symbol) {
        self.write("(import ");
        self.write(self.sym(path));
        match items {
            ImportClause::Glob => self.write(" *"),
            ImportClause::GlobAs(name) => {
                self.write(" * as ");
                self.write(self.sym(*name));
            }
            ImportClause::Items(list) => {
                self.write(" [");
                for (i, item) in list.iter().enumerate() {
                    if i > 0 {
                        self.write_char(' ');
                    }
                    self.write(self.sym(item.name));
                    if let Some(alias) = item.alias {
                        self.write(" as ");
                        self.write(self.sym(alias));
                    }
                }
                self.write_char(']');
            }
        }
        self.write_char(')');
    }

    fn print_export(&mut self, items: &[ExportItem], path: Symbol) {
        self.write("(export ");
        self.write(self.sym(path));
        self.write(" [");
        for (i, item) in items.iter().enumerate() {
            if i > 0 {
                self.write_char(' ');
            }
            self.write(self.sym(item.name));
            if let Some(alias) = item.alias {
                self.write(" as ");
                self.write(self.sym(alias));
            }
        }
        self.write("])");
    }

    fn print_optional_guard(&mut self, guard: Option<Idx<Expr>>) {
        if let Some(g) = guard {
            self.write(" (guard ");
            self.print_expr(g);
            self.write_char(')');
        }
    }

    fn print_fn_def(&mut self, node: &FnDefView<'_>) {
        self.write("(fn_def");
        self.print_attrs(node.attrs);
        self.print_modifiers(node.modifiers);
        self.write_char(' ');
        self.write(self.sym(node.name));
        self.write(" [");
        self.print_ty_params(node.ty_params);
        self.write_char(']');
        self.indent += 2;
        self.newline_indent();
        self.print_params_list(node.params);
        if let Some(ret) = node.ret_ty {
            self.newline_indent();
            self.write("(ret ");
            self.print_ty(ret);
            self.write_char(')');
        }
        if !node.where_clause.is_empty() {
            self.newline_indent();
            self.print_where_clause(node.where_clause);
        }
        if let Some(b) = node.body {
            self.newline_indent();
            self.print_expr(b);
        }
        self.indent -= 2;
        self.write_char(')');
    }

    fn print_lambda(
        &mut self,
        attrs: &[Attr],
        ty_params: &[TyParam],
        params: &[Param],
        ret_ty: Option<&Ty>,
        where_clause: &[Constraint],
        body: Idx<Expr>,
    ) {
        self.write("(lambda");
        self.print_attrs(attrs);
        self.write(" [");
        self.print_ty_params(ty_params);
        self.write_char(']');
        self.indent += 2;
        self.newline_indent();
        self.print_params_list(params);
        if let Some(ret) = ret_ty {
            self.newline_indent();
            self.write("(ret ");
            self.print_ty(ret);
            self.write_char(')');
        }
        if !where_clause.is_empty() {
            self.newline_indent();
            self.print_where_clause(where_clause);
        }
        self.newline_indent();
        self.print_expr(body);
        self.indent -= 2;
        self.write_char(')');
    }

    fn print_where_clause(&mut self, constraints: &[Constraint]) {
        self.write("(where");
        for c in constraints {
            self.write_char(' ');
            self.write(self.sym(c.ty_var));
            self.write(" satisfies ");
            self.print_ty(&c.bound);
        }
        self.write_char(')');
    }

    fn print_class_def(
        &mut self,
        name: Symbol,
        ty_params: &[TyParam],
        supers: &[Ty],
        members: &[ClassMember],
    ) {
        self.write("(class ");
        self.write(self.sym(name));
        self.write(" [");
        self.print_ty_params(ty_params);
        self.write_char(']');
        self.write(" (supers");
        for s in supers {
            self.write_char(' ');
            self.print_ty(s);
        }
        self.write_char(')');
        self.indent += 2;
        self.write(" (members");
        for m in members {
            self.newline_indent();
            self.print_class_member(m);
        }
        self.write_char(')');
        self.indent -= 2;
        self.write_char(')');
    }

    fn print_given_def(
        &mut self,
        class_app: &Ty,
        constraints: &[Constraint],
        members: &[ClassMember],
    ) {
        self.write("(given ");
        self.print_ty(class_app);
        if !constraints.is_empty() {
            self.write_char(' ');
            self.print_where_clause(constraints);
        }
        self.indent += 2;
        self.write(" (members");
        for m in members {
            self.newline_indent();
            self.print_class_member(m);
        }
        self.write_char(')');
        self.indent -= 2;
        self.write_char(')');
    }

    fn print_class_member(&mut self, member: &ClassMember) {
        match member {
            ClassMember::Method(idx) => {
                self.print_expr(*idx);
            }
            ClassMember::Law { name, params, body, .. } => {
                self.write("(law ");
                self.write(self.sym(*name));
                self.write_char(' ');
                self.print_params_list(params);
                self.write_char(' ');
                self.print_expr(*body);
                self.write_char(')');
            }
        }
    }

    fn print_record(
        &mut self,
        attrs: &[Attr],
        modifiers: &[Modifier],
        name: Option<Symbol>,
        ty_params: &[TyParam],
        fields: &[RecField],
    ) {
        self.write("(record");
        self.print_attrs(attrs);
        self.print_modifiers(modifiers);
        if let Some(n) = name {
            self.write_char(' ');
            self.write(self.sym(n));
        }
        self.write(" [");
        self.print_ty_params(ty_params);
        self.write_char(']');
        self.indent += 2;
        for field in fields {
            self.newline_indent();
            self.print_rec_field(field);
        }
        self.indent -= 2;
        self.write_char(')');
    }

    fn print_choice(
        &mut self,
        attrs: &[Attr],
        modifiers: &[Modifier],
        name: Option<Symbol>,
        ty_params: &[TyParam],
        variants: &[ChoiceVariant],
    ) {
        self.write("(choice");
        self.print_attrs(attrs);
        self.print_modifiers(modifiers);
        if let Some(n) = name {
            self.write_char(' ');
            self.write(self.sym(n));
        }
        self.write(" [");
        self.print_ty_params(ty_params);
        self.write_char(']');
        self.indent += 2;
        for variant in variants {
            self.newline_indent();
            self.print_choice_variant(variant);
        }
        self.indent -= 2;
        self.write_char(')');
    }

    fn print_bind(
        &mut self,
        attrs: &[Attr],
        modifiers: &[Modifier],
        kind: BindKind,
        pat: &Pat,
        ty: Option<&Ty>,
        init: Option<Idx<Expr>>,
    ) {
        self.write("(bind ");
        self.print_attrs(attrs);
        self.print_modifiers(modifiers);
        match kind {
            BindKind::Const => self.write("const"),
            BindKind::Var => self.write("var"),
        }
        self.write_char(' ');
        self.print_pat(pat);
        if let Some(t) = ty {
            self.write_char(' ');
            self.print_ty(t);
        }
        if let Some(init_expr) = init {
            self.write_char(' ');
            self.print_expr(init_expr);
        }
        self.write_char(')');
    }

    fn print_postfix(&mut self, base: Idx<Expr>, op: &PostfixOp) {
        match op {
            PostfixOp::Call { args, .. } => {
                self.write("(call ");
                self.print_expr(base);
                self.write(" [");
                self.space_separated_exprs(args);
                self.write("])");
            }
            PostfixOp::Index { args, .. } => {
                self.write("(index ");
                self.print_expr(base);
                self.write(" [");
                self.space_separated_exprs(args);
                self.write("])");
            }
            PostfixOp::Field { name, .. } => {
                self.write("(field ");
                self.print_expr(base);
                self.write_char(' ');
                self.write(self.sym(*name));
                self.write_char(')');
            }
            PostfixOp::RecDot { fields, .. } => {
                self.write("(rec_dot ");
                self.print_expr(base);
                self.write(" [");
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        self.write_char(' ');
                    }
                    self.print_rec_lit_field(field);
                }
                self.write("])");
            }
            PostfixOp::OptField { name, .. } => {
                self.write("(opt-field ");
                self.print_expr(base);
                self.write_char(' ');
                self.write(self.sym(*name));
                self.write_char(')');
            }
            PostfixOp::As { ty, .. } => {
                self.write("(as ");
                self.print_expr(base);
                self.write_char(' ');
                self.print_ty(ty);
                self.write_char(')');
            }
        }
    }

    fn print_ty(&mut self, ty: &Ty) {
        match ty {
            Ty::Named { name, args, .. } if args.is_empty() => {
                self.write(self.sym(*name));
            }
            Ty::Named { name, args, .. } => {
                self.write("(apply ");
                self.write(self.sym(*name));
                self.write(" [");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.write_char(' ');
                    }
                    self.print_ty(arg);
                }
                self.write("])");
            }
            Ty::Arrow { params, ret, .. } => {
                self.write("(fn [");
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        self.write_char(' ');
                    }
                    self.print_ty(p);
                }
                self.write("] ");
                self.print_ty(ret);
                self.write_char(')');
            }
            Ty::Prod { elements, .. } if elements.is_empty() => {
                self.write("unit_ty");
            }
            Ty::Prod { elements, .. } => {
                self.write("(prod [");
                for (i, e) in elements.iter().enumerate() {
                    if i > 0 {
                        self.write_char(' ');
                    }
                    self.print_ty(e);
                }
                self.write("])");
            }
            Ty::Arr { element, size, .. } => {
                self.write("(arr ");
                self.print_ty(element);
                if let Some(sz) = size {
                    self.write_char(' ');
                    self.print_expr(*sz);
                }
                self.write_char(')');
            }
            Ty::Var { name, .. } => {
                self.write(self.sym(*name));
            }
            Ty::Option { inner, .. } => {
                self.write("(option ");
                self.print_ty(inner);
                self.write_char(')');
            }
            Ty::Error { .. } => self.write("error_ty"),
        }
    }

    fn print_pat(&mut self, pat: &Pat) {
        match pat {
            Pat::Ident {
                name, suffix: None, ..
            } => {
                self.write(self.sym(*name));
            }
            Pat::Ident {
                name,
                suffix: Some(PatSuffix::Positional { args, .. }),
                ..
            } => {
                self.write("(pat_sum ");
                self.write(self.sym(*name));
                self.write(" [");
                for (i, a) in args.iter().enumerate() {
                    if i > 0 {
                        self.write_char(' ');
                    }
                    self.print_pat(a);
                }
                self.write("])");
            }
            Pat::Ident {
                name,
                suffix: Some(PatSuffix::Named { fields, .. }),
                ..
            } => {
                self.write("(pat_sum_named ");
                self.write(self.sym(*name));
                self.write(" [");
                for (i, f) in fields.iter().enumerate() {
                    if i > 0 {
                        self.write_char(' ');
                    }
                    self.print_pat_field(f);
                }
                self.write("])");
            }
            Pat::Lit { value, .. } => {
                self.write("(pat_lit ");
                self.print_lit(value);
                self.write_char(')');
            }
            Pat::Wild { .. } => self.write("_"),
            Pat::Prod { elements, .. } => {
                self.write("(pat_prod [");
                for (i, e) in elements.iter().enumerate() {
                    if i > 0 {
                        self.write_char(' ');
                    }
                    self.print_pat(e);
                }
                self.write("])");
            }
            Pat::Arr { elements, .. } => {
                self.write("(pat_arr [");
                for (i, e) in elements.iter().enumerate() {
                    if i > 0 {
                        self.write_char(' ');
                    }
                    self.print_pat(e);
                }
                self.write("])");
            }
            Pat::AnonRec { fields, .. } => {
                self.write("(pat_rec [");
                for (i, f) in fields.iter().enumerate() {
                    if i > 0 {
                        self.write_char(' ');
                    }
                    self.print_pat_field(f);
                }
                self.write("])");
            }
            Pat::Or { alternatives, .. } => {
                self.write("(pat_or [");
                for (i, a) in alternatives.iter().enumerate() {
                    if i > 0 {
                        self.write_char(' ');
                    }
                    self.print_pat(a);
                }
                self.write("])");
            }
            Pat::DotPrefix { name, args, .. } => {
                self.write("(dot-pat ");
                self.write(self.sym(*name));
                for a in args {
                    self.write_char(' ');
                    self.print_pat(a);
                }
                self.write_char(')');
            }
            Pat::Error { .. } => self.write("error_pat"),
        }
    }

    fn print_pat_field(&mut self, f: &PatField) {
        self.write("(field ");
        if f.mutable {
            self.write("var ");
        }
        self.write(self.sym(f.name));
        if let Some(ref p) = f.pat {
            self.write_char(' ');
            self.print_pat(p);
        }
        self.write_char(')');
    }

    fn print_lit(&mut self, value: &LitValue) {
        match value {
            LitValue::Int(n) => {
                self.write("(lit_int ");
                self.write(&n.to_string());
                self.write_char(')');
            }
            LitValue::Float(f) => {
                self.write("(lit_float ");
                self.write(&f.to_string());
                self.write_char(')');
            }
            LitValue::Str(s) => {
                self.write("(lit_str \"");
                self.write(self.sym(*s));
                self.write("\")");
            }
            LitValue::Char(c) => {
                self.write("(lit_char '");
                self.write_char(*c);
                self.write("')");
            }
        }
    }

    fn print_cond(&mut self, cond: &Cond) {
        match cond {
            Cond::Expr(idx) => self.print_expr(*idx),
            Cond::Case { pat, init, .. } => {
                self.write("(case ");
                self.print_pat(pat);
                self.write(" := ");
                self.print_expr(*init);
                self.write_char(')');
            }
        }
    }

    fn print_elif(&mut self, elif: &ElifBranch) {
        self.write(" (elif ");
        self.print_cond(&elif.cond);
        self.print_optional_guard(elif.guard);
        self.write_char(' ');
        self.print_expr(elif.body);
        self.write_char(')');
    }

    fn print_match_arm(&mut self, arm: &MatchArm) {
        self.write("(case ");
        self.print_pat(&arm.pat);
        self.print_optional_guard(arm.guard);
        self.write(" => ");
        self.print_expr(arm.body);
        self.write_char(')');
    }

    fn print_attrs(&mut self, attrs: &[Attr]) {
        for attr in attrs {
            self.write(" #[");
            self.write(self.sym(attr.name));
            if !attr.args.is_empty() {
                self.write_char('(');
                for (i, arg) in attr.args.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_attr_arg(arg);
                }
                self.write_char(')');
            }
            self.write_char(']');
        }
    }

    fn print_attr_arg(&mut self, arg: &AttrArg) {
        match arg {
            AttrArg::Named {
                name,
                value: Some(v),
                ..
            } => {
                self.write(self.sym(*name));
                self.write(" = ");
                self.print_lit(v);
            }
            AttrArg::Named {
                name, value: None, ..
            } => {
                self.write(self.sym(*name));
            }
            AttrArg::Value { value: v, .. } => self.print_lit(v),
        }
    }

    fn print_modifiers(&mut self, modifiers: &[Modifier]) {
        for m in modifiers {
            match m {
                Modifier::Export => self.write(" export"),
                Modifier::Opaque => self.write(" opaque"),
                Modifier::Native(None) => self.write(" native"),
                Modifier::Native(Some(abi)) => {
                    self.write(" native(\"");
                    self.write(self.sym(*abi));
                    self.write("\")");
                }
            }
        }
    }

    fn print_ty_params(&mut self, ty_params: &[TyParam]) {
        for (i, tp) in ty_params.iter().enumerate() {
            if i > 0 {
                self.write_char(' ');
            }
            self.write(self.sym(tp.name));
            if !tp.bounds.is_empty() {
                self.write(": ");
                for (j, b) in tp.bounds.iter().enumerate() {
                    if j > 0 {
                        self.write(" + ");
                    }
                    self.print_ty(b);
                }
            }
        }
    }

    fn print_params_list(&mut self, params: &[Param]) {
        self.write("(params");
        for param in params {
            self.write_char(' ');
            self.print_param(param);
        }
        self.write_char(')');
    }

    fn print_param(&mut self, param: &Param) {
        self.write("(param ");
        if param.mutable {
            self.write("var ");
        }
        self.write(self.sym(param.name));
        if let Some(ref ty) = param.ty {
            self.write_char(' ');
            self.print_ty(ty);
        }
        self.write_char(')');
    }

    fn print_rec_field(&mut self, field: &RecField) {
        self.write("(field ");
        if field.mutable {
            self.write("var ");
        }
        self.write(self.sym(field.name));
        if let Some(ref ty) = field.ty {
            self.write_char(' ');
            self.print_ty(ty);
        }
        self.write_char(')');
    }

    fn print_rec_lit_field(&mut self, field: &FieldInit) {
        match field {
            FieldInit::Named {
                mutable,
                name,
                value,
                ..
            } => {
                self.write("(field ");
                if *mutable {
                    self.write("var ");
                }
                self.write(self.sym(*name));
                self.write_char(' ');
                self.print_expr(*value);
                self.write_char(')');
            }
            FieldInit::Spread { expr, .. } => {
                self.write("(spread ");
                self.print_expr(*expr);
                self.write_char(')');
            }
        }
    }

    fn print_choice_variant(&mut self, variant: &ChoiceVariant) {
        self.write("(variant ");
        self.write(self.sym(variant.name));
        match &variant.payload {
            None => {}
            Some(VariantPayload::Positional(tys)) => {
                self.write(" (");
                for (i, ty) in tys.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_ty(ty);
                }
                self.write_char(')');
            }
            Some(VariantPayload::Named(fields)) => {
                self.write(" {");
                for (i, f) in fields.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.print_rec_field(f);
                }
                self.write_char('}');
            }
            Some(VariantPayload::Discriminant(lit)) => {
                self.write(" := ");
                self.print_lit(lit);
            }
        }
        self.write_char(')');
    }
}

const fn binop_str(op: BinOp) -> &'static str {
    match op {
        BinOp::Add => "+",
        BinOp::Sub => "-",
        BinOp::Mul => "*",
        BinOp::Div => "/",
        BinOp::Rem => "%",
        BinOp::BitOr => "|",
        BinOp::BitXor => "^",
        BinOp::BitAnd => "&",
        BinOp::Shl => "shl",
        BinOp::Shr => "shr",
        BinOp::And => "and",
        BinOp::Or => "or",
        BinOp::Xor => "xor",
        BinOp::Eq => "=",
        BinOp::NotEq => "/=",
        BinOp::Lt => "<",
        BinOp::Gt => ">",
        BinOp::LtEq => "<=",
        BinOp::GtEq => ">=",
        BinOp::In => "in",
        BinOp::Range => "..",
        BinOp::RangeExcl => "..<",
        BinOp::Cons => "::",
        BinOp::NilCoalesce => "??",
    }
}

const fn prefix_str(op: PrefixOp) -> &'static str {
    match op {
        PrefixOp::Neg => "-",
        PrefixOp::Not => "not",
        PrefixOp::Deref => "!",
        PrefixOp::AddrOf => "@",
        PrefixOp::BitNot => "~",
    }
}

#[cfg(test)]
mod tests;
