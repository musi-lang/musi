use music_found::Ident;

use crate::{TyId, TyList};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyKind {
    Named {
        name: Ident,
        args: TyList,
    },
    Arrow {
        from: TyId,
        to: TyId,
    },
    EffectArrow {
        from: TyId,
        to: TyId,
    },
    Sum(TyList),
    Product(TyList),
    Mut(TyId),
    Option(TyId),
    Pi {
        name: Ident,
        param_ty: TyId,
        ret_ty: TyId,
    },
    Tuple(TyList),
    Array {
        dims: Vec<Dim>,
        elem: TyId,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Dim {
    Lit(i64),
    Var(Ident),
    Inferred,
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
