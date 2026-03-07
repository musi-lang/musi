//! Printer methods for control flow: if, match, loops, jumps, and imports.

use musi_shared::{Idx, Symbol};

use crate::ast::{Cond, ElifBranch, ExportItem, Expr, ImportClause, MatchArm, Pat};

use super::Printer;

impl Printer<'_> {
    pub(super) fn print_if(
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

    pub(super) fn print_match(&mut self, scrutinee: Idx<Expr>, arms: &[MatchArm]) {
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

    pub(super) fn print_while(
        &mut self,
        cond: &Cond,
        guard: Option<Idx<Expr>>,
        body: Idx<Expr>,
    ) {
        self.write("(while ");
        self.print_cond(cond);
        self.print_optional_guard(guard);
        self.write_char(' ');
        self.print_expr(body);
        self.write_char(')');
    }

    pub(super) fn print_loop(&mut self, body: Idx<Expr>, post_cond: Option<&Cond>) {
        self.write("(loop ");
        self.print_expr(body);
        if let Some(pc) = post_cond {
            self.write(" (post_while ");
            self.print_cond(pc);
            self.write_char(')');
        }
        self.write_char(')');
    }

    pub(super) fn print_for(
        &mut self,
        pat: &Pat,
        iter: Idx<Expr>,
        guard: Option<Idx<Expr>>,
        body: Idx<Expr>,
    ) {
        self.write("(for ");
        self.print_pat(pat);
        self.write_char(' ');
        self.print_expr(iter);
        self.print_optional_guard(guard);
        self.write_char(' ');
        self.print_expr(body);
        self.write_char(')');
    }

    pub(super) fn print_optional_tag(&mut self, tag: &str, value: Option<Idx<Expr>>) {
        self.write_char('(');
        self.write(tag);
        if let Some(v) = value {
            self.write_char(' ');
            self.print_expr(v);
        }
        self.write_char(')');
    }

    pub(super) fn print_break(&mut self, label: Option<Symbol>, value: Option<Idx<Expr>>) {
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

    pub(super) fn print_cycle(&mut self, label: Option<Symbol>, guard: Option<Idx<Expr>>) {
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

    pub(super) fn print_import(&mut self, items: &ImportClause, path: Symbol) {
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
                self.print_symbol_alias_list(list.iter().map(|i| (i.name, i.alias)));
                self.write_char(']');
            }
        }
        self.write_char(')');
    }

    pub(super) fn print_export(&mut self, items: &[ExportItem], path: Symbol) {
        self.write("(export ");
        self.write(self.sym(path));
        self.write(" [");
        self.print_symbol_alias_list(items.iter().map(|i| (i.name, i.alias)));
        self.write("])");
    }

    pub(super) fn print_optional_guard(&mut self, guard: Option<Idx<Expr>>) {
        if let Some(g) = guard {
            self.write(" (guard ");
            self.print_expr(g);
            self.write_char(')');
        }
    }

    fn print_symbol_alias_list(
        &mut self,
        items: impl IntoIterator<Item = (Symbol, Option<Symbol>)>,
    ) {
        for (i, (name, alias)) in items.into_iter().enumerate() {
            if i > 0 {
                self.write_char(' ');
            }
            self.write(self.sym(name));
            if let Some(a) = alias {
                self.write(" as ");
                self.write(self.sym(a));
            }
        }
    }

    pub(super) fn print_cond(&mut self, cond: &Cond) {
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

    pub(super) fn print_elif(&mut self, elif: &ElifBranch) {
        self.write(" (elif ");
        self.print_cond(&elif.cond);
        self.print_optional_guard(elif.guard);
        self.write_char(' ');
        self.print_expr(elif.body);
        self.write_char(')');
    }

    pub(super) fn print_match_arm(&mut self, arm: &MatchArm) {
        self.write("(case ");
        self.print_pat(&arm.pat);
        self.print_optional_guard(arm.guard);
        self.write(" => ");
        self.print_expr(arm.body);
        self.write_char(')');
    }
}
