use musi_ast::{AstArena, ChoiceCaseItem, ExprId, ExprKind, Ident, Prog, StmtKind, TyExprId};
use musi_basic::diagnostic::DiagnosticBag;
use musi_basic::interner::Interner;

use crate::phase2::{BindCtx, resolve_ty_expr};
use crate::semantic::SemanticModel;
use crate::symbol::{SymbolKind, SymbolTable};
use crate::unifier::Unifier;

pub fn resolve(
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

    for stmt_id in &prog.stmts {
        let stmt = arena.stmts.get(*stmt_id);
        let StmtKind::Expr(expr_id) = &stmt.kind;
        resolve_expr(&mut ctx, *expr_id);
    }
}

fn resolve_expr(ctx: &mut BindCtx<'_>, expr_id: ExprId) {
    let expr = ctx.arena.exprs.get(expr_id);
    match &expr.kind {
        ExprKind::Bind { init, .. } => resolve_expr(ctx, *init),
        ExprKind::RecordDef { fields, .. } => {
            for field in fields {
                if let Some(ty_id) = field.ty {
                    resolve_field(ctx, field.name, ty_id, field.mutable);
                }
            }
        }
        ExprKind::ChoiceDef { cases, .. } => {
            for case in cases {
                for field_item in &case.fields {
                    if let ChoiceCaseItem::Field(field) = field_item
                        && let Some(ty_id) = field.ty
                    {
                        resolve_field(ctx, field.name, ty_id, field.mutable);
                    }
                }
            }
        }
        ExprKind::Alias { name, ty, .. } => {
            let resolved = resolve_ty_expr(ctx, *ty);
            if let Some(sym_id) = ctx.symbols.lookup(*name)
                && let Some(sym) = ctx.symbols.get_mut(sym_id)
            {
                sym.ty = resolved;
            }
        }
        _ => {}
    }
}

fn resolve_field(ctx: &mut BindCtx<'_>, name: Ident, ty_id: TyExprId, mutable: bool) {
    let field_ty = resolve_ty_expr(ctx, ty_id);
    _ = ctx.define_and_record(name, SymbolKind::Field, field_ty, name.span, mutable);
}
