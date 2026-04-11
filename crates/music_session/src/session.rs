mod cache;
mod compile;
mod graph;
mod store;

#[cfg(test)]
use music_base::Diag;
use music_base::{Source, SourceId, SourceMap};
use music_names::{Interner, Symbol};

use crate::api::{SessionOptions, SessionStats};

use self::graph::SessionGraph;
use self::store::SessionStore;

pub struct Session {
    options: SessionOptions,
    interner: Interner,
    store: SessionStore,
    graph: SessionGraph,
    stats: SessionStats,
    #[cfg(test)]
    test_hooks: SessionTestHooks,
}

#[cfg(test)]
#[derive(Default)]
struct SessionTestHooks {
    ir_failure: Option<Box<[Diag]>>,
    emit_failure: Option<Box<[Diag]>>,
}

impl Session {
    #[must_use]
    pub fn new(options: SessionOptions) -> Self {
        Self {
            options,
            interner: Interner::new(),
            store: SessionStore::new(),
            graph: SessionGraph::default(),
            stats: SessionStats::default(),
            #[cfg(test)]
            test_hooks: SessionTestHooks::default(),
        }
    }

    #[must_use]
    pub const fn stats(&self) -> &SessionStats {
        &self.stats
    }

    #[must_use]
    pub fn source_map(&self) -> &SourceMap {
        &self.store.sources
    }

    #[must_use]
    pub fn source(&self, id: SourceId) -> Option<&Source> {
        self.store.sources.get(id)
    }

    #[must_use]
    pub fn resolve_symbol(&self, symbol: Symbol) -> &str {
        self.interner.resolve(symbol)
    }

    #[cfg(test)]
    pub(crate) fn inject_ir_failure_for_tests(&mut self, diags: impl Into<Box<[Diag]>>) {
        self.test_hooks.ir_failure = Some(diags.into());
    }

    #[cfg(test)]
    pub(crate) fn inject_emit_failure_for_tests(&mut self, diags: impl Into<Box<[Diag]>>) {
        self.test_hooks.emit_failure = Some(diags.into());
    }
}
