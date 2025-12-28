use musi_basic::arena::NodeId;

use crate::node::{Cond, Expr, Pat, Stmt, TyExpr};

pub use musi_basic::types::Ident;

pub type TyExprId = NodeId<TyExpr>;

pub type PatId = NodeId<Pat>;

pub type ExprId = NodeId<Expr>;
pub type CondId = NodeId<Cond>;

pub type StmtId = NodeId<Stmt>;
