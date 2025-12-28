use musi_ast::{StmtId, StmtKind};

use super::BindCtx;
use super::expr::bind_expr;

pub fn bind_stmt(ctx: &mut BindCtx<'_>, stmt_id: StmtId) {
    let stmt = ctx.arena.stmts.get(stmt_id);
    match &stmt.kind {
        StmtKind::Expr(expr_id) => {
            let _ = bind_expr(ctx, *expr_id);
        }
    }
}
