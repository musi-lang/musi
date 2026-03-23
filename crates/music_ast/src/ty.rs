use music_found::Ident;

use crate::TyId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyKind {
    Named {
        name: Ident,
        args: Vec<TyId>,
    },
    Arrow {
        from: TyId,
        to: TyId,
    },
    EffectArrow {
        from: TyId,
        to: TyId,
    },
    Union(Vec<TyId>),
    Mut(TyId),
    Option(TyId),
    Pi {
        name: Ident,
        param_ty: TyId,
        ret_ty: TyId,
    },
    Tuple(Vec<TyId>),
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
