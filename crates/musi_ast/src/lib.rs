pub mod node;
pub mod visitor;

pub use node::*;
pub use visitor::*;

pub type Ident = u32;
pub type Idents = Vec<Ident>;
pub type OptIdent = Option<Ident>;

pub type ExprPtr = Box<Expr>;
pub type CondPtr = Box<Cond>;
pub type TypPtr = Box<Typ>;
pub type OptExprPtr = Option<ExprPtr>;
pub type OptExpr = Option<Expr>;
pub type OptTyp = Option<Typ>;

pub type ExprList = Vec<Expr>;
pub type TypList = Vec<Typ>;
pub type PatList = Vec<Pat>;
pub type StmtList = Vec<Stmt>;
pub type SumCaseItemList = Vec<SumCaseItem>;
pub type AttrList = Vec<Attr>;
pub type AttrArgList = Vec<AttrArg>;
pub type FieldList = Vec<Field>;
