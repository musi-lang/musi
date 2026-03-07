//! Printer methods for expressions: block, array, postfix, binary, prefix.

use musi_shared::Idx;

use crate::ast::{ArrayItem, BinOp, Expr, FieldInit, PostfixOp, PrefixOp};

use super::{binop_str, prefix_str, Printer};

impl Printer<'_> {
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
        self.write_space_separated(items, |p, item| match *item {
            ArrayItem::Single(e) => p.print_expr(e),
            ArrayItem::Spread(e) => {
                p.write("(spread ");
                p.print_expr(e);
                p.write_char(')');
            }
        });
        self.write("])");
    }

    pub(super) fn print_anon_rec(&mut self, fields: &[FieldInit]) {
        self.write("(anon_rec [");
        self.write_space_separated(fields, super::Printer::print_rec_lit_field);
        self.write("])");
    }

    pub(super) fn print_binary(
        &mut self,
        op: BinOp,
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

    pub(super) fn print_prefix(&mut self, op: PrefixOp, operand: Idx<Expr>) {
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
                self.space_separated_exprs(self.module.ctx.expr_lists.get_slice(*args));
                self.write("])");
            }
            PostfixOp::Index { args, .. } => {
                self.write("(index ");
                self.print_expr(base);
                self.write(" [");
                self.space_separated_exprs(self.module.ctx.expr_lists.get_slice(*args));
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
                self.write_space_separated(fields, super::Printer::print_rec_lit_field);
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
