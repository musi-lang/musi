//! Printer methods for types, patterns, and shared formatting helpers.

use musi_shared::Idx;

use crate::ast::{
    Attr, AttrArg, ChoiceVariant, Expr, LitValue, Modifier, Param, Pat, PatField, PatSuffix,
    PostfixOp, RecField, Ty, TyParam, VariantPayload,
};

use super::Printer;

impl<'a> Printer<'a> {
    pub(super) fn print_postfix(&mut self, base: Idx<Expr>, op: &PostfixOp) {
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

    pub(super) fn print_ty(&mut self, ty: &Ty) {
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

    pub(super) fn print_pat(&mut self, pat: &Pat) {
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

    pub(super) fn print_pat_field(&mut self, f: &PatField) {
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

    pub(super) fn print_lit(&mut self, value: &LitValue) {
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

    pub(super) fn print_params_list(&mut self, params: &[Param]) {
        self.write("(params");
        for param in params {
            self.write_char(' ');
            self.print_param(param);
        }
        self.write_char(')');
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
