use music_ast::data::AstData;
use music_ast::expr::{BinOp, ExprKind, PiecewiseArm, PwGuard};
use music_ast::walk::map_expr_children;
use music_ast::ExprId;
use music_found::{Span, Spanned};

/// Desugar AST-only nodes into HIR-only nodes.
///
/// Two transforms:
/// 1. `Piecewise(arms)` becomes nested `Branch { cond, then_br, else_br }`
/// 2. `BinOp(PipeRight, lhs, rhs)` becomes `App(rhs, [lhs])`
///
/// After lowering, no `Piecewise` or `PipeRight` nodes remain reachable
/// from `ast.root`.
pub fn lower(ast: &mut AstData) {
    let root = ast.root.clone();
    ast.root = root.iter().map(|&id| lower_expr(ast, id)).collect();
}

fn lower_expr(ast: &mut AstData, expr_id: ExprId) -> ExprId {
    // Bottom-up: lower all children first via the generic walker.
    let lowered = map_expr_children(ast, expr_id, &mut |ast, id| lower_expr(ast, id));

    // Then transform this node if it matches a desugaring rule.
    let spanned = ast.exprs.get(lowered);
    let span = spanned.span;
    let kind = spanned.kind.clone();

    match kind {
        ExprKind::Piecewise(ref arms) => lower_piecewise(ast, arms, span),
        ExprKind::BinOp(BinOp::PipeRight, lhs, rhs) => ast
            .exprs
            .alloc(Spanned::new(ExprKind::App(rhs, vec![lhs]), span)),
        _ => lowered,
    }
}

fn lower_piecewise(ast: &mut AstData, arms: &[PiecewiseArm], span: Span) -> ExprId {
    if arms.is_empty() {
        return ast
            .exprs
            .alloc(Spanned::new(ExprKind::TupleLit(Vec::new()), span));
    }

    let last = &arms[arms.len() - 1];

    let mut result = match last.guard {
        PwGuard::Wildcard => last.value,
        PwGuard::Expr(cond) => {
            let unit = ast
                .exprs
                .alloc(Spanned::new(ExprKind::TupleLit(Vec::new()), span));
            ast.exprs.alloc(Spanned::new(
                ExprKind::Branch {
                    cond,
                    then_br: last.value,
                    else_br: unit,
                },
                span,
            ))
        }
    };

    for arm in arms.iter().rev().skip(1) {
        match arm.guard {
            PwGuard::Expr(cond) => {
                result = ast.exprs.alloc(Spanned::new(
                    ExprKind::Branch {
                        cond,
                        then_br: arm.value,
                        else_br: result,
                    },
                    span,
                ));
            }
            PwGuard::Wildcard => {
                result = arm.value;
            }
        }
    }

    result
}
