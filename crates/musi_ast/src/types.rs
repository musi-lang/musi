//! Type aliases for AST nodes.

use crate::node::{Attr, AttrArg, Cond, Expr, Field, Pat, Stmt, SumCaseItem, TyExpr};

// ============================================================================
// IDENTIFIERS
// ============================================================================

pub type Ident = u32;
pub type Idents = Vec<Ident>;
pub type OptIdent = Option<Ident>;

// ============================================================================
// TYPE EXPRESSIONS
// ============================================================================

pub type TyExprPtr = Box<TyExpr>;
pub type TyExprs = Vec<TyExpr>;
pub type OptTyExpr = Option<TyExpr>;

// ============================================================================
// PATTERNS
// ============================================================================

pub type Pats = Vec<Pat>;

// ============================================================================
// EXPRESSIONS
// ============================================================================

pub type ExprPtr = Box<Expr>;
pub type Exprs = Vec<Expr>;
pub type OptExprPtr = Option<ExprPtr>;
pub type OptExpr = Option<Expr>;
pub type CondPtr = Box<Cond>;

// ============================================================================
// STATEMENTS
// ============================================================================

pub type Stmts = Vec<Stmt>;

// ============================================================================
// SUPPORT TYPES
// ============================================================================

pub type Fields = Vec<Field>;
pub type SumCaseItems = Vec<SumCaseItem>;
pub type Attrs = Vec<Attr>;
pub type AttrArgs = Vec<AttrArg>;
