use musi_core::NodeId;

use crate::node::{Cond, Expr, Pat, Stmt, TyExpr};

pub type TyExprId = NodeId<TyExpr>;

pub type PatId = NodeId<Pat>;

pub type ExprId = NodeId<Expr>;
pub type CondId = NodeId<Cond>;

pub type StmtId = NodeId<Stmt>;
