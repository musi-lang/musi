//! Printer methods for declarations: fn, lambda, record, choice, bind, class, given.

use musi_shared::{Idx, Symbol};

use crate::ast::{
    Attr, BindKind, ChoiceVariant, ClassMember, Constraint, Expr, FieldInit, Modifier, Param,
    Pat, RecField, Ty, TyParam,
};

use super::{FnDefView, Printer};

impl<'a> Printer<'a> {
    pub(super) fn print_fn_def(&mut self, node: &FnDefView<'_>) {
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

    pub(super) fn print_lambda(
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

    pub(super) fn print_where_clause(&mut self, constraints: &[Constraint]) {
        self.write("(where");
        for c in constraints {
            self.write_char(' ');
            self.write(self.sym(c.ty_var));
            self.write(" satisfies ");
            self.print_ty(&c.bound);
        }
        self.write_char(')');
    }

    pub(super) fn print_class_def(
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

    pub(super) fn print_given_def(
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

    pub(super) fn print_class_member(&mut self, member: &ClassMember) {
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

    pub(super) fn print_record(
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

    pub(super) fn print_choice(
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

    pub(super) fn print_bind(
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

    pub(super) fn print_rec_lit_field(&mut self, field: &FieldInit) {
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
}
