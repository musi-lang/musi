//! Printer methods for types and patterns.

use crate::ast::{LitValue, Pat, PatField, PatSuffix, Ty};

use super::Printer;

impl<'a> Printer<'a> {
    pub(super) fn print_ty(&mut self, ty: &Ty) {
        match ty {
            Ty::Named { name, args, .. } if args.is_empty() => {
                self.write(self.sym(*name));
            }
            Ty::Named { name, args, .. } => {
                self.write("(apply ");
                self.write(self.sym(*name));
                self.write(" [");
                self.write_space_separated(args, |p, a| p.print_ty(a));
                self.write("])");
            }
            Ty::Arrow { params, ret, .. } => {
                self.write("(fn [");
                self.write_space_separated(params, |p, t| p.print_ty(t));
                self.write("] ");
                self.print_ty(ret);
                self.write_char(')');
            }
            Ty::Prod { elements, .. } if elements.is_empty() => {
                self.write("unit_ty");
            }
            Ty::Prod { elements, .. } => {
                self.write("(prod [");
                self.write_space_separated(elements, |p, e| p.print_ty(e));
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
                self.write_space_separated(args, |p, a| p.print_pat(a));
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
                self.write_space_separated(fields, |p, f| p.print_pat_field(f));
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
                self.write_space_separated(elements, |p, e| p.print_pat(e));
                self.write("])");
            }
            Pat::Arr { elements, .. } => {
                self.write("(pat_arr [");
                self.write_space_separated(elements, |p, e| p.print_pat(e));
                self.write("])");
            }
            Pat::AnonRec { fields, .. } => {
                self.write("(pat_rec [");
                self.write_space_separated(fields, |p, f| p.print_pat_field(f));
                self.write("])");
            }
            Pat::Or { alternatives, .. } => {
                self.write("(pat_or [");
                self.write_space_separated(alternatives, |p, a| p.print_pat(a));
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
}
