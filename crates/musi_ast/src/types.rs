use musi_basic::arena::NodeId;

use crate::node::{Attr, AttrArg, ChoiceCaseItem, Cond, Expr, Field, Pat, Stmt, TyExpr};

pub use musi_basic::types::{Ident, Idents, OptIdent};

// ============================================================================
// TYPE EXPRESSIONS
// ============================================================================

pub type TyExprId = NodeId<TyExpr>;
pub type TyExprIds = Vec<TyExprId>;
pub type OptTyExprId = Option<TyExprId>;

// ============================================================================
// PATTERNS
// ============================================================================

pub type PatId = NodeId<Pat>;
pub type PatIds = Vec<PatId>;

// ============================================================================
// EXPRESSIONS
// ============================================================================

pub type ExprId = NodeId<Expr>;
pub type ExprIds = Vec<ExprId>;
pub type OptExprId = Option<ExprId>;
pub type CondId = NodeId<Cond>;

// ============================================================================
// STATEMENTS
// ============================================================================

pub type StmtId = NodeId<Stmt>;
pub type StmtIds = Vec<StmtId>;

// ============================================================================
// SUPPORT TYPES
// ============================================================================

pub type Fields = Vec<Field>;
pub type ChoiceCaseItems = Vec<ChoiceCaseItem>;
pub type Attrs = Vec<Attr>;
pub type AttrArgs = Vec<AttrArg>;
