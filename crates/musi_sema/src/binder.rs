use std::mem;

use musi_ast::{AstArena, ExprId, Prog, StmtKind};
use musi_basic::diagnostic::{Diagnostic, DiagnosticBag};
use musi_basic::error::IntoMusiError;
use musi_basic::interner::Interner;

use crate::builtins::Builtins;
use crate::phase2::{BindCtx, bind_expr};
use crate::semantic::SemanticModel;
use crate::symbol::SymbolTable;
use crate::unifier::Unifier;
use crate::{SemaErrorKind, SymbolId, SymbolKind, phase1};

#[must_use]
pub fn bind(
    arena: &AstArena,
    interner: &mut Interner,
    prog: &Prog,
    builtins: &Builtins,
) -> (SemanticModel, SymbolTable, DiagnosticBag) {
    let mut model = SemanticModel::new(arena.exprs.len(), arena.pats.len(), arena.ty_exprs.len());
    let mut symbols = SymbolTable::new();
    builtins.register(&mut symbols);

    let mut unifier = Unifier::new();
    let mut diags = DiagnosticBag::default();
    let mut deferred = vec![];

    let mut ctx = BindCtx::new(
        arena,
        interner,
        &mut model,
        &mut symbols,
        &mut unifier,
        &mut diags,
        &mut deferred,
    );

    phase1::collect(ctx.arena, prog, ctx.symbols, ctx.model);
    phase1::resolve(&mut ctx, prog);
    bind_stmts(&mut ctx, prog);

    finalize_types(arena, &mut model, &unifier);
    report_unused(interner, &symbols, &mut diags);

    (model, symbols, diags)
}

fn bind_stmts(ctx: &mut BindCtx<'_>, prog: &Prog) {
    for stmt_id in &prog.stmts {
        let stmt = ctx.arena.stmts.get(*stmt_id);
        let StmtKind::Expr(expr_id) = &stmt.kind;
        let _ = bind_expr(ctx, *expr_id);
    }

    while !ctx.deferred.is_empty() {
        let tasks = mem::take(ctx.deferred);

        for task in tasks {
            ctx.reenter_scope(task.scope);
            ctx.in_fn = true;
            let body_ty = bind_expr(ctx, task.body);
            let body_span = ctx.arena.exprs.get(task.body).span;
            ctx.unify_or_err(&task.expected_ret, &body_ty, body_span);
        }
    }
}

fn finalize_types(arena: &AstArena, model: &mut SemanticModel, unifier: &Unifier) {
    for idx in 0..arena.exprs.len() {
        let expr_id = ExprId::new(u32::try_from(idx).expect("index overflow"));
        if let Some(ty) = model.type_of_expr(expr_id).cloned() {
            let finalized = unifier.finalize(&ty);
            model.set_expr_type(expr_id, finalized);
        }
    }
}

fn report_unused(interner: &Interner, symbols: &SymbolTable, diags: &mut DiagnosticBag) {
    for i in 0..symbols.len() {
        let id = SymbolId::new(u32::try_from(i).expect("symbol ID overflow"));
        let Some(sym) = symbols.get(id) else {
            continue;
        };

        match sym.kind {
            SymbolKind::Local | SymbolKind::Param | SymbolKind::Fn => {}
            _ => continue,
        }

        if symbols.is_used(id) {
            continue;
        }

        let name = interner.resolve(sym.name.id);
        if name.starts_with('_') {
            continue;
        }

        let diag = Diagnostic::from(
            SemaErrorKind::UnusedName(name.to_owned()).into_musi_error(sym.def_span),
        );
        diags.add(diag);
    }
}

#[cfg(test)]
mod tests;
