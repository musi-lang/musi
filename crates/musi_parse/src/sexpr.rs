//! Deterministic S-expression printer for the Musi AST.
//!
//! Output is diff-friendly and intended for snapshot tests.
//! Spans are omitted so that output does not depend on source positions.

mod decls;
mod exprs;
mod stmts;
mod types_and_pats;

use musi_shared::{Idx, Interner, Symbol};

use crate::ast::{
    Attr, BinOp, Constraint, Expr, Modifier, Param, ParsedModule, PrefixOp, Ty, TyParam,
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
}

// -- Free helpers ------------------------------------------------------------

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
