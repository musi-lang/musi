use musi_ast::{AstArena, ExprId, ExprKind, PatId, PatKind, Prog, StmtId, StmtKind};
use musi_core::{Interner, MusiResult};

use crate::{DefId, DefKind, DefTable, HirExprId, HirExprKind, HirModule, Resolver, errors};

pub struct Lowerer<'a> {
    ast: &'a AstArena,
    hir: HirModule,
    defs: DefTable,
    resolver: Resolver<'a>,
}

impl<'a> Lowerer<'a> {
    fn new(ast: &'a AstArena, interner: &'a Interner) -> Self {
        Self {
            ast,
            hir: HirModule::new(),
            defs: DefTable::new(),
            resolver: Resolver::new(interner),
        }
    }

    fn lower_expr(&mut self, expr_id: ExprId) -> MusiResult<HirExprId> {
        let expr = self.ast.exprs.get(expr_id);
        let span = expr.span;

        let kind = match &expr.kind {
            ExprKind::Lit(lit) => HirExprKind::Lit(lit.clone()),
            ExprKind::Ident(name) => {
                let def_id = self.resolver.resolve(*name)?;
                HirExprKind::Ref(def_id)
            }
            ExprKind::Tuple(elems) => {
                let hir_elems = self.lower_exprs(elems)?;
                HirExprKind::Tuple(hir_elems)
            }
            ExprKind::Array(elems) => {
                let hir_elems = self.lower_exprs(elems)?;
                HirExprKind::Array(hir_elems)
            }
            ExprKind::Binary { op, lhs, rhs } => {
                let hir_lhs = self.lower_expr(*lhs)?;
                let hir_rhs = self.lower_expr(*rhs)?;
                HirExprKind::Binary {
                    op: *op,
                    lhs: hir_lhs,
                    rhs: hir_rhs,
                }
            }
            ExprKind::Unary { op, operand } => {
                let hir_operand = self.lower_expr(*operand)?;
                HirExprKind::Unary {
                    op: *op,
                    operand: hir_operand,
                }
            }
            ExprKind::Block { stmts, expr } => self.lower_block(stmts, expr.as_ref())?,
            ExprKind::Binding {
                mutable, pat, init, ..
            } => {
                let hir_init = self.lower_expr(*init)?;
                let def_id = self.lower_bind_pat(*pat)?;
                HirExprKind::Binding {
                    def: def_id,
                    mutable: *mutable,
                    init: hir_init,
                }
            }
            ExprKind::Return(expr) => {
                let hir_expr = self.lower_optional_expr(expr.as_ref())?;
                HirExprKind::Return(hir_expr)
            }
            ExprKind::Break(expr) => {
                let hir_expr = self.lower_optional_expr(expr.as_ref())?;
                HirExprKind::Break(hir_expr)
            }
            ExprKind::Cycle => HirExprKind::Cycle,
            ExprKind::Import(path) => HirExprKind::Import(*path),

            _ => return Err(errors::unsupported_expr(&expr.kind, span)),
        };
        Ok(self.hir.alloc_expr(kind, span))
    }

    fn lower_block(
        &mut self,
        stmts: &[musi_ast::StmtId],
        expr: Option<&ExprId>,
    ) -> MusiResult<HirExprKind> {
        self.resolver.push_scope();
        let hir_stmts = self.lower_stmts(stmts)?;
        let hir_expr = self.lower_optional_expr(expr)?;
        self.resolver.pop_scope();
        Ok(HirExprKind::Block {
            stmts: hir_stmts,
            expr: hir_expr,
        })
    }

    fn lower_optional_expr(&mut self, expr: Option<&ExprId>) -> MusiResult<Option<HirExprId>> {
        match expr {
            Some(e) => Ok(Some(self.lower_expr(*e)?)),
            None => Ok(None),
        }
    }

    fn lower_exprs(&mut self, exprs: &[ExprId]) -> MusiResult<Vec<HirExprId>> {
        exprs.iter().map(|e| self.lower_expr(*e)).collect()
    }

    fn lower_stmts(&mut self, stmts: &[StmtId]) -> MusiResult<Vec<HirExprId>> {
        let mut hir_stmts = vec![];
        for stmt_id in stmts {
            let stmt = self.ast.stmts.get(*stmt_id);
            match &stmt.kind {
                StmtKind::Expr(expr_id) => {
                    hir_stmts.push(self.lower_expr(*expr_id)?);
                }
            }
        }
        Ok(hir_stmts)
    }

    fn lower_bind_pat(&mut self, pat_id: PatId) -> MusiResult<DefId> {
        let pat = self.ast.pats.get(pat_id);
        if let PatKind::Ident(name) = &pat.kind {
            return Ok(self.resolver.define(&mut self.defs, DefKind::Val, *name));
        }
        Err(errors::unsupported_pattern(&pat.kind, pat.span))
    }
}

/// # Errors
/// Returns error if lowering fails.
pub fn lower_module(
    prog: &Prog,
    ast: &AstArena,
    interner: &Interner,
) -> MusiResult<(HirModule, DefTable)> {
    let mut lowerer = Lowerer::new(ast, interner);

    for stmt_id in &prog.stmts {
        let stmt = ast.stmts.get(*stmt_id);
        match &stmt.kind {
            StmtKind::Expr(expr_id) => {
                let hir_id = lowerer.lower_expr(*expr_id)?;
                lowerer.hir.top_level.push(hir_id);
            }
        }
    }

    Ok((lowerer.hir, lowerer.defs))
}
