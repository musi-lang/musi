use musi_ast::{AstArena, ExprId, Prog, StmtKind};
use musi_basic::diagnostic::DiagnosticBag;
use musi_basic::interner::Interner;

use crate::builtins::Builtins;
use crate::phase1;
use crate::phase2::{BindCtx, bind_expr};
use crate::semantic::SemanticModel;
use crate::symbol::SymbolTable;
use crate::unifier::Unifier;
use rayon::prelude::*;

#[must_use]
pub fn bind(
    arena: &AstArena,
    interner: &Interner,
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

    (model, symbols, diags)
}

fn bind_prog(
    arena: &AstArena,
    interner: &Interner,
    prog: &Prog,
    symbols: &mut SymbolTable,
    model: &mut SemanticModel,
    unifier: &mut Unifier,
    diags: &mut DiagnosticBag,
) {
    let mut ctx = BindCtx {
        arena,
        interner,
        model,
        symbols,
        unifier,
        diags,
        in_loop: false,
        in_fn: false,
    };

    let tasks: Vec<_> = prog
        .stmts
        .iter()
        .map(|stmt_id| {
            let stmt = arena.stmts.get(*stmt_id);
            let StmtKind::Expr(expr_id) = &stmt.kind;
            *expr_id
        })
        .collect();

    let ctx_arena = ctx.arena;
    let ctx_interner = ctx.interner;
    let in_loop = ctx.in_loop;
    let in_fn = ctx.in_fn;

    let results: Vec<_> = tasks
        .into_par_iter()
        .map(|expr_id| {
            let mut model = ctx.model.fork();
            let mut symbols = ctx.symbols.fork();
            let mut unifier = ctx.unifier.fork();
            let mut diags = DiagnosticBag::default();

            let mut forked = BindCtx {
                arena: ctx_arena,
                interner: ctx_interner,
                model: &mut model,
                symbols: &mut symbols,
                unifier: &mut unifier,
                diags: &mut diags,
                in_loop,
                in_fn,
            };

            let _ = bind_expr(&mut forked, expr_id);

            (model, unifier, symbols, diags)
        })
        .collect();

    for (m, u, s, d) in results {
        ctx.merge_forked(m, u, s, d);
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

#[cfg(test)]
mod tests;
