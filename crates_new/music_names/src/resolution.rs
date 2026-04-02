use std::collections::HashMap;

use music_arena::{Arena, Idx};
use music_base::{SourceId, Span};

use crate::Symbol;

pub type SymbolSlice = Box<[Symbol]>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident {
    pub name: Symbol,
    pub span: Span,
}

impl Ident {
    #[must_use]
    pub const fn new(name: Symbol, span: Span) -> Self {
        Self { name, span }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NameSite {
    pub source_id: SourceId,
    pub span: Span,
}

impl NameSite {
    #[must_use]
    pub const fn new(source_id: SourceId, span: Span) -> Self {
        Self { source_id, span }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NameBindingKind {
    Prelude,
    Import { opaque: bool },
    Let,
    Param,
    PiBinder,
    TypeParam,
    PatternBind,
    HandleClauseResult,
    HandleClauseParam,
}

#[derive(Debug, Clone, Copy)]
pub struct NameBinding {
    pub name: Symbol,
    pub site: NameSite,
    pub kind: NameBindingKind,
}

pub type NameBindingId = Idx<NameBinding>;

#[derive(Debug, Default)]
pub struct NameResolution {
    pub bindings: Arena<NameBinding>,
    pub refs: HashMap<NameSite, NameBindingId>,
}

impl NameResolution {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn alloc_binding(&mut self, binding: NameBinding) -> NameBindingId {
        self.bindings.alloc(binding)
    }

    pub fn record_ref(&mut self, site: NameSite, binding: NameBindingId) {
        let _prev = self.refs.insert(site, binding);
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
