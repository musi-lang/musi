use music_arena::Arena;
use music_shared::Spanned;

use crate::ExprList;
use crate::common::Attr;
use crate::expr::ExprKind;
use crate::pat::PatKind;
use crate::ty::TyKind;

#[derive(Debug)]
pub struct AstData {
    pub exprs: Arena<Spanned<ExprKind>>,
    pub pats: Arena<Spanned<PatKind>>,
    pub types: Arena<Spanned<TyKind>>,
    pub attrs: Arena<Spanned<Attr>>,
    pub root: ExprList,
}

impl AstData {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            exprs: Arena::new(),
            pats: Arena::new(),
            types: Arena::new(),
            attrs: Arena::new(),
            root: Vec::new(),
        }
    }

    #[must_use]
    pub fn with_capacity(hint: usize) -> Self {
        Self {
            exprs: Arena::with_capacity(hint),
            pats: Arena::with_capacity(hint / 4),
            types: Arena::with_capacity(hint / 4),
            attrs: Arena::with_capacity(hint / 8),
            root: Vec::with_capacity(hint / 8),
        }
    }
}

impl Default for AstData {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
