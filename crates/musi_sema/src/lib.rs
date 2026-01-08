mod checker;
pub mod errors;
mod inferer;
pub mod table;
mod ty;
mod ty_env;
mod unifier;

use musi_ast::{AstArena, Prog, StmtKind};
use musi_core::{Interner, MusiResult};

pub use checker::Checker;
pub use inferer::Inferer;
pub use table::UnificationTable;
pub use ty::{Ty, TyArena, TyId, TyKind};
pub use ty_env::TyEnv;
pub use unifier::Unifier;
#[cfg(test)]
mod test_ctx;

#[derive(Debug)]
pub struct TypedModule {
    pub ty_arena: TyArena,
    pub env: TyEnv,
}

/// # Errors
/// Returns error if semantic analysis fails.
pub fn analyze(ast: &AstArena, interner: &Interner, prog: &Prog) -> MusiResult<TypedModule> {
    let mut ty_arena = TyArena::new();
    let mut env = TyEnv::new();
    let mut table = UnificationTable::new();

    let mut checker = Checker::new(ast);
    let mut inferer = Inferer::new(ast, interner, &mut ty_arena, &mut env, &mut table);

    for stmt_id in &prog.stmts {
        let stmt = ast.stmts.get(*stmt_id);
        let StmtKind::Expr(expr_id) = &stmt.kind;
        checker.check_expr(*expr_id)?;
        let _ = inferer.infer_expr(*expr_id)?;
    }

    Ok(TypedModule { ty_arena, env })
}
