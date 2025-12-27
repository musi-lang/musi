use crate::symbol::SymbolId;
use crate::ty_repr::TyRepr;
use crate::{Scope, SemanticToken, Symbol};

pub type TyReprs = Vec<TyRepr>;
pub type TyReprPtr = Box<TyRepr>;

pub type Scopes = Vec<Scope>;

pub type Symbols = Vec<Symbol>;
pub type SymbolIds = Vec<SymbolId>;

pub type SemanticTokens = Vec<SemanticToken>;
