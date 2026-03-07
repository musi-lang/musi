//! Printer methods for expressions: block, array, postfix, binary, prefix.

use musi_shared::Idx;

use crate::ast::{ArrayItem, Expr, FieldInit, PostfixOp};

use super::{binop_str, prefix_str, Printer};

impl<'a> Printer<'a> {
    pub(super) fn print_block(&mut self, stmts: &[Idx<Expr>], tail: Option<Idx<Expr>>) {
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

    pub(super) fn print_array(&mut self, items: &[ArrayItem]) {
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

    pub(super) fn print_anon_rec(&mut self, fields: &[FieldInit]) {
        self.write("(anon_rec [");
        for (i, field) in fields.iter().enumerate() {
            if i > 0 {
                self.write_char(' ');
            }
            self.print_rec_lit_field(field);
        }
        self.write("])");
    }

    pub(super) fn print_binary(
        &mut self,
        op: crate::ast::BinOp,
        lhs: Idx<Expr>,
        rhs: Idx<Expr>,
    ) {
        self.write("(binary ");
        self.write(binop_str(op));
        self.write_char(' ');
        self.print_expr(lhs);
        self.write_char(' ');
        self.print_expr(rhs);
        self.write_char(')');
    }

    pub(super) fn print_prefix(&mut self, op: crate::ast::PrefixOp, operand: Idx<Expr>) {
        self.write("(prefix ");
        self.write(prefix_str(op));
        self.write_char(' ');
        self.print_expr(operand);
        self.write_char(')');
    }

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
}
