use std::collections::HashMap;

use musi_core::{Name, Span};

use crate::ty::TyId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolKind {
    Fn,
    Local,
    Param,
    Type,
    Field,
    Variant,
    Builtin,
}

#[derive(Debug, Clone, Copy)]
pub struct Symbol {
    pub name: Name,
    pub kind: SymbolKind,
    pub ty: TyId,
    pub def_span: Span,
}

impl Symbol {
    #[must_use]
    pub const fn new(name: Name, kind: SymbolKind, ty: TyId, def_span: Span) -> Self {
        Self {
            name,
            kind,
            ty,
            def_span,
        }
    }
}

#[derive(Debug, Default)]
pub struct SymbolTable {
    defs: HashMap<Name, Symbol>,
    refs: HashMap<Span, Name>,
}

impl SymbolTable {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn define(&mut self, sym: Symbol) {
        let _ = self.defs.insert(sym.name, sym);
    }

    pub fn record_ref(&mut self, span: Span, name: Name) {
        let _ = self.refs.insert(span, name);
    }

    #[must_use]
    pub fn get(&self, name: Name) -> Option<&Symbol> {
        self.defs.get(&name)
    }

    #[must_use]
    pub fn symbol_at_offset(&self, offset: u32) -> Option<(Name, Span)> {
        for (span, name) in &self.refs {
            if span.lo <= offset && offset <= span.hi {
                return Some((*name, *span));
            }
        }
        for sym in self.defs.values() {
            let span = sym.def_span;
            if span.lo <= offset && offset <= span.hi {
                return Some((sym.name, span));
            }
        }
        None
    }

    #[must_use]
    pub fn references_of(&self, target: Name) -> Vec<Span> {
        let mut spans = Vec::new();
        if let Some(sym) = self.defs.get(&target) {
            spans.push(sym.def_span);
        }
        for (span, name) in &self.refs {
            if *name == target {
                spans.push(*span);
            }
        }
        spans
    }

    pub fn iter_defs(&self) -> impl Iterator<Item = &Symbol> {
        self.defs.values()
    }

    pub fn iter_refs(&self) -> impl Iterator<Item = (&Span, &Name)> {
        self.refs.iter()
    }

    #[must_use]
    pub fn is_unused(&self, name: Name) -> bool {
        !self.refs.values().any(|n| *n == name)
    }
}
