use std::collections::{BTreeMap, BTreeSet};

use music_base::{SourceId, SourceMap};
use music_emit::EmittedModule;
use music_ir::IrModule;
use music_module::ModuleKey;
use music_resolve::ResolvedModule;
use music_sema::SemaModule;
use music_syntax::ParsedSource as SyntaxParsedSource;

use crate::api::{ParsedModule, SessionError};

use super::Session;

#[derive(Default)]
pub(super) struct ModuleRecord {
    pub(super) text: String,
    pub(super) revision: u64,
    pub(super) source_id: Option<SourceId>,
    pub(super) parsed: Option<ParsedModule>,
    pub(super) parsed_source: Option<SyntaxParsedSource>,
    pub(super) resolved: Option<ResolvedModule>,
    pub(super) sema: Option<SemaModule>,
    pub(super) ir: Option<IrModule>,
    pub(super) emitted: Option<EmittedModule>,
    pub(super) static_imports: BTreeSet<ModuleKey>,
}

#[derive(Default)]
pub(super) struct SessionStore {
    pub(super) sources: SourceMap,
    pub(super) modules: BTreeMap<ModuleKey, ModuleRecord>,
    next_revision: u64,
}

impl SessionStore {
    pub(super) fn new() -> Self {
        Self {
            next_revision: 1,
            ..Self::default()
        }
    }
}

impl Session {
    /// Registers or replaces the source text for a module and invalidates cached downstream phases.
    ///
    /// # Errors
    ///
    /// Returns an error if the module text cannot be interned into the session source map.
    pub fn set_module_text(
        &mut self,
        key: &ModuleKey,
        text: impl Into<String>,
    ) -> Result<(), SessionError> {
        let revision = self.store.next_revision;
        self.store.next_revision = self.store.next_revision.saturating_add(1);
        let record = self.store.modules.entry(key.clone()).or_default();
        record.text = text.into();
        record.revision = revision;
        record.source_id = None;
        record.parsed = None;
        record.parsed_source = None;
        record.resolved = None;
        record.sema = None;
        record.ir = None;
        record.emitted = None;
        self.invalidate_dependents(key);
        self.graph.entry_programs.clear();
        let _ = self.ensure_source_id(key)?;
        Ok(())
    }

    pub(super) fn ensure_source_id(&mut self, key: &ModuleKey) -> Result<SourceId, SessionError> {
        if let Some(source_id) = self.module_record(key)?.source_id {
            return Ok(source_id);
        }
        let text = self.module_record(key)?.text.clone();
        let source_id = self.store.sources.add(key.as_str(), text)?;
        self.module_record_mut(key)?.source_id = Some(source_id);
        Ok(source_id)
    }

    pub(super) fn module_record(&self, key: &ModuleKey) -> Result<&ModuleRecord, SessionError> {
        self.store
            .modules
            .get(key)
            .ok_or_else(|| SessionError::ModuleNotRegistered { key: key.clone() })
    }

    pub(super) fn module_record_mut(
        &mut self,
        key: &ModuleKey,
    ) -> Result<&mut ModuleRecord, SessionError> {
        self.store
            .modules
            .get_mut(key)
            .ok_or_else(|| SessionError::ModuleNotRegistered { key: key.clone() })
    }
}
