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

    phase1::collect(arena, prog, &mut symbols, &mut model);
    phase1::resolve(
        arena,
        interner,
        prog,
        &mut symbols,
        &mut model,
        &mut unifier,
        &mut diags,
    );

    bind_prog(
        arena,
        interner,
        prog,
        &mut symbols,
        &mut model,
        &mut unifier,
        &mut diags,
    );
    finalize_types(arena, &mut model, &unifier);
    report_unused(interner, &symbols, &mut diags);

    (model, symbols, diags)
}

fn bind_prog(
    arena: &AstArena,
    interner: &mut Interner,
    prog: &Prog,
    symbols: &mut SymbolTable,
    model: &mut SemanticModel,
    unifier: &mut Unifier,
    diags: &mut DiagnosticBag,
) {
    let mut deferred = vec![];
    let mut ctx = BindCtx {
        arena,
        interner,
        model,
        symbols,
        unifier,
        diags,
        deferred: &mut deferred,
        in_loop: false,
        in_fn: false,
    };

    for stmt_id in &prog.stmts {
        let stmt = arena.stmts.get(*stmt_id);
        let StmtKind::Expr(expr_id) = &stmt.kind;
        let _ = bind_expr(&mut ctx, *expr_id);
    }

    while !ctx.deferred.is_empty() {
        let tasks = mem::take(ctx.deferred);

        let ctx_arena = ctx.arena;
        let ctx_interner = &mut *ctx.interner;
        let in_loop = ctx.in_loop;

        let results: Vec<_> = tasks
            .into_iter()
            .map(|task| {
                let mut model = ctx.model.fork();
                let mut symbols = ctx.symbols.fork();
                let mut unifier = ctx.unifier.fork();
                let mut diags = DiagnosticBag::default();
                let mut next_deferred = vec![];

                let mut forked = BindCtx {
                    arena: ctx_arena,
                    interner: ctx_interner,
                    model: &mut model,
                    symbols: &mut symbols,
                    unifier: &mut unifier,
                    diags: &mut diags,
                    deferred: &mut next_deferred,
                    in_loop,
                    in_fn: true,
                };

                forked.reenter_scope(task.scope);
                let body_ty = bind_expr(&mut forked, task.body);
                forked.unify_or_err(
                    &task.expected_ret,
                    &body_ty,
                    ctx_arena.exprs.get(task.body).span,
                );

                (model, unifier, symbols, diags, next_deferred)
            })
            .collect();

        for (m, u, s, d, nd) in results {
            ctx.merge_forked(m, u, s, d, nd);
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
