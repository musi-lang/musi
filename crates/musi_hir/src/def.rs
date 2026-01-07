use musi_core::{Span, Symbol};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(pub u32);

impl DefId {
    pub const DUMMY: Self = Self(u32::MAX);

    fn to_usize(self) -> usize {
        usize::try_from(self.0).expect("DefId overflow")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefKind {
    Fn,
    Val,
    Var,
    Type,
    Record,
    Choice,
    Param,
    Import,
}

#[derive(Debug, Clone)]
pub struct Def {
    pub id: DefId,
    pub kind: DefKind,
    pub name: musi_core::Symbol,
    pub span: Span,
}

#[derive(Debug, Default)]
pub struct DefTable {
    defs: Vec<Def>,
}

impl DefTable {
    #[must_use]
    pub const fn new() -> Self {
        Self { defs: vec![] }
    }

    pub fn insert(&mut self, kind: DefKind, name: Symbol, span: Span) -> DefId {
        let len = self.defs.len();
        let id = u32::try_from(len).map_or_else(|_| DefId::DUMMY, DefId);
        self.defs.push(Def {
            id,
            kind,
            name,
            span,
        });
        id
    }

    #[must_use]
    pub fn get(&self, id: DefId) -> Option<&Def> {
        self.defs.get(id.to_usize())
    }
}
