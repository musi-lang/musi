mod cache;
mod compile;
mod graph;
mod store;

use music_names::Interner;

use crate::api::{SessionOptions, SessionStats};

use self::graph::SessionGraph;
use self::store::SessionStore;

pub struct Session {
    options: SessionOptions,
    interner: Interner,
    store: SessionStore,
    graph: SessionGraph,
    stats: SessionStats,
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
        }
    }

    #[must_use]
    pub const fn stats(&self) -> &SessionStats {
        &self.stats
    }
}
