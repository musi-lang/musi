use crate::symbol::SymbolId;
use crate::ty_repr::TyRepr;

pub type TyReprs = Vec<TyRepr>;
pub type OptTyRepr = Option<TyRepr>;
pub type OptTyReprs = Vec<OptTyRepr>;
pub type TyReprPtr = Box<TyRepr>;

pub type SymbolIds = Vec<SymbolId>;
pub type OptSymbolId = Option<SymbolId>;
pub type OptSymbolIds = Vec<OptSymbolId>;
