use std::collections::HashMap;

use music_basic::{SourceId, Span};
use music_storage::{Arena, Idx};

use crate::Symbol;

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

pub type NameBindingId = Idx<NameBinding>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NameBinding {
    pub name: Symbol,
    pub site: NameSite,
    pub kind: NameBindingKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NameBindingKind {
    Prelude,
    Import { opaque: bool },
    Let,
    Param,
    TypeParam,
    PiBinder,
    HandleClauseResult,
    HandleClauseParam,
    PatternBind,
}

#[derive(Debug, Default)]
pub struct NameResolution {
    pub bindings: Arena<NameBinding>,
    pub refs: HashMap<NameSite, NameBindingId>,
}

impl NameResolution {
    #[must_use]
    pub fn new() -> Self {
        Self {
            bindings: Arena::new(),
            refs: HashMap::new(),
        }
    }

    #[must_use]
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
