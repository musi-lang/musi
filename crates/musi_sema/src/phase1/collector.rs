use musi_ast::{AstArena, ExprId, ExprKind, Ident, PatKind, Prog, StmtKind};

use crate::semantic::SemanticModel;
use crate::symbol::{SymbolKind, SymbolTable};
use crate::ty_repr::TyRepr;

pub fn collect(
    arena: &AstArena,
    prog: &Prog,
    symbols: &mut SymbolTable,
    model: &mut SemanticModel,
) {
    for stmt_id in &prog.stmts {
        let stmt = arena.stmts.get(*stmt_id);
        let StmtKind::Expr(expr_id) = &stmt.kind;
        collect_expr(arena, *expr_id, symbols, model);
    }
}

fn collect_expr(
    arena: &AstArena,
    expr_id: ExprId,
    symbols: &mut SymbolTable,
    model: &mut SemanticModel,
) {
    let expr = arena.exprs.get(expr_id);
    match &expr.kind {
        ExprKind::Bind { pat, init, .. } => {
            let init_expr = arena.exprs.get(*init);
            match &init_expr.kind {
                ExprKind::RecordDef { name: None, .. } | ExprKind::ChoiceDef { name: None, .. } => {
                    if let PatKind::Ident(ident) = &arena.pats.get(*pat).kind {
                        collect_symbol_type(ident, symbols, model);
                        return;
                    }
                }
                _ => {}
            }
            collect_expr(arena, *init, symbols, model);
        }
        ExprKind::RecordDef {
            name: Some(ident), ..
        } => {
            collect_symbol_type(ident, symbols, model);
        }
        ExprKind::ChoiceDef {
            name: Some(ident),
            cases,
            ..
        } => {
            collect_symbol_type(ident, symbols, model);
            for case in cases {
                collect_symbol_variant(&case.name, symbols, model);
            }
        }
        ExprKind::ChoiceDef {
            name: None, cases, ..
        } => {
            for case in cases {
                collect_symbol_variant(&case.name, symbols, model);
            }
        }
        ExprKind::Alias { name, .. } => {
            collect_symbol_type(name, symbols, model);
        }
        ExprKind::Fn { sig, .. } => {
            if let Some(name) = &sig.name {
                collect_symbol_fn(name, symbols, model);
            }
        }
        ExprKind::Extern { fns, .. } => {
            for sig in fns {
                if let Some(name) = &sig.name {
                    collect_symbol_fn(name, symbols, model);
                }
            }
        }
        _ => {}
    }
}

fn collect_symbol_type(ident: &Ident, symbols: &mut SymbolTable, model: &mut SemanticModel) {
    if let Ok(sym_id) = symbols.define(*ident, SymbolKind::Type, TyRepr::unit(), ident.span, false)
    {
        model.set_ident_symbol(*ident, sym_id);
    }
}

fn collect_symbol_variant(ident: &Ident, symbols: &mut SymbolTable, model: &mut SemanticModel) {
    if let Ok(sym_id) = symbols.define(
        *ident,
        SymbolKind::Variant,
        TyRepr::unit(),
        ident.span,
        false,
    ) {
        model.set_ident_symbol(*ident, sym_id);
    }
}

fn collect_symbol_fn(ident: &Ident, symbols: &mut SymbolTable, model: &mut SemanticModel) {
    if let Ok(sym_id) = symbols.define(*ident, SymbolKind::Fn, TyRepr::unit(), ident.span, false) {
        model.set_ident_symbol(*ident, sym_id);
    }
}
