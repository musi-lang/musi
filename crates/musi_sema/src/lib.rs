mod checker;
pub mod errors;
mod inferer;
pub mod symbol;
mod table;
mod ty;
mod ty_env;
mod unifier;

use musi_ast::{AstArena, Prog, StmtKind};
use musi_core::{Diagnostic, DiagnosticBag, Interner};

pub use checker::Checker;
pub use inferer::Inferer;
pub use symbol::{Symbol, SymbolKind, SymbolTable};
pub use table::UnificationTable;
pub use ty::{Ty, TyArena, TyId, TyKind};
pub use ty_env::TyEnv;
pub use unifier::Unifier;

#[cfg(test)]
mod test_ctx;

#[must_use]
pub fn bind(
    ast: &AstArena,
    interner: &Interner,
    prog: &Prog,
) -> (SymbolTable, TyArena, DiagnosticBag) {
    let mut ty_arena = TyArena::new();
    let mut env = TyEnv::new();
    let mut table = UnificationTable::new();
    let mut symbols = SymbolTable::new();
    let mut bag = DiagnosticBag::default();

    let mut checker = Checker::new(ast);
    let mut inferer = Inferer::new(
        ast,
        interner,
        &mut ty_arena,
        &mut env,
        &mut table,
        &mut symbols,
    );

    for stmt_id in &prog.stmts {
        let stmt = ast.stmts.get(*stmt_id);
        let StmtKind::Expr(expr_id) = &stmt.kind;
        if let Err(e) = checker.check_expr(*expr_id) {
            bag.add(Diagnostic::error(&e.message, e.span));
            continue;
        }
        if let Err(e) = inferer.infer_expr(*expr_id) {
            bag.add(Diagnostic::error(&e.message, e.span));
        }
    }

    (symbols, ty_arena, bag)
}
