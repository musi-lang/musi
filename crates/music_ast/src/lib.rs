pub mod common;
pub mod data;
pub mod expr;
pub mod pat;
pub mod ty;
pub mod walk;

use music_arena::Idx;
use music_shared::{Ident, Spanned};

use common::{Attr, Param};
use expr::ExprKind;
use pat::PatKind;
use ty::TyKind;

pub type ExprId = Idx<Spanned<ExprKind>>;
pub type PatId = Idx<Spanned<PatKind>>;
pub type TyId = Idx<Spanned<TyKind>>;
pub type AttrId = Idx<Spanned<Attr>>;
pub type ExprList = Vec<ExprId>;
pub type PatList = Vec<PatId>;
pub type TyList = Vec<TyId>;
pub type ParamList = Vec<Param>;
pub type AttrList = Vec<AttrId>;
pub type IdentList = Vec<Ident>;
