//! Shared formatting helpers: attributes, modifiers, type params, params, and field/variant printing.

use crate::ast::{
    Attr, AttrArg, ChoiceVariant, Constraint, Modifier, Param, RecField, Ty, TyParam,
    VariantPayload,
};

use super::Printer;

impl<'a> Printer<'a> {
    pub(super) fn print_attrs(&mut self, attrs: &[Attr]) {
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

    pub(super) fn print_modifiers(&mut self, modifiers: &[Modifier]) {
        for m in modifiers {
            match m {
                Modifier::Export => self.write(" export"),
                Modifier::Opaque => self.write(" opaque"),
                Modifier::Extrin(None) => self.write(" extrin"),
                Modifier::Extrin(Some(abi)) => {
                    self.write(" extrin(\"");
                    self.write(self.sym(*abi));
                    self.write("\")");
                }
            }
        }
    }

    pub(super) fn print_ty_params(&mut self, ty_params: &[TyParam]) {
        self.write_space_separated(ty_params, |p, tp| {
            p.write(p.sym(tp.name));
            if !tp.bounds.is_empty() {
                p.write(": ");
                for (j, b) in tp.bounds.iter().enumerate() {
                    if j > 0 {
                        p.write(" + ");
                    }
                    p.print_ty(b);
                }
            }
        });
    }

    pub(super) fn print_params_list(&mut self, params: &[Param]) {
        self.write("(params");
        for param in params {
            self.write_char(' ');
            self.print_param(param);
        }
        self.write_char(')');
    }

    pub(super) fn print_fn_signature(
        &mut self,
        ty_params: &[TyParam],
        params: &[Param],
        ret_ty: Option<&Ty>,
        where_clause: &[Constraint],
    ) {
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
    }

    pub(super) fn print_param(&mut self, param: &Param) {
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

    pub(super) fn print_rec_field(&mut self, field: &RecField) {
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

    pub(super) fn print_choice_variant(&mut self, variant: &ChoiceVariant) {
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
