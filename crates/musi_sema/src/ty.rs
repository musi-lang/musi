use musi_core::{Arena, NodeId, Span, Symbol};

pub type TyId = NodeId<Ty>;

#[derive(Debug, Clone)]
pub struct Ty {
    pub kind: TyKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyKind {
    Any,
    Never,
    Int,
    Real,
    String,
    Rune,
    Bool,
    Unit,
    Tuple(Vec<TyId>),
    Array(TyId),
    Optional(TyId),
    Ptr(TyId),
    Range(TyId),
    Named(Symbol),
    Record { fields: Vec<(Symbol, TyId)> },
    Fn { params: Vec<TyId>, ret: TyId },
    Union(Vec<TyId>),
    Var(u32),
}

#[derive(Debug, Default)]
pub struct TyArena {
    types: Arena<Ty>,
}

impl TyArena {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            types: Arena::new(),
        }
    }

    pub fn alloc(&mut self, kind: TyKind, span: Span) -> TyId {
        self.types.alloc(Ty { kind, span })
    }

    #[must_use]
    pub fn get(&self, id: TyId) -> &Ty {
        self.types.get(id)
    }

    #[must_use]
    pub fn get_mut(&mut self, id: TyId) -> &mut Ty {
        self.types.get_mut(id)
    }
}
