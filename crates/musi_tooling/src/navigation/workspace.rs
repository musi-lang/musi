use std::collections::HashMap;
use std::fmt;
use std::path::{Path, PathBuf};

use super::symbol_analysis::SymbolAnalysis;
use super::types::{ToolLocation, ToolReferenceLens};

#[derive(Default)]
pub struct NavigationWorkspace {
    analyses: HashMap<PathBuf, SymbolAnalysis>,
}

impl fmt::Debug for NavigationWorkspace {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_struct("NavigationWorkspace")
            .field("cached_paths", &self.analyses.len())
            .finish()
    }
}

impl NavigationWorkspace {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn clear(&mut self) {
        self.analyses.clear();
    }

    pub fn invalidate_path(&mut self, _path: &Path) {
        self.analyses.clear();
    }

    #[cfg(test)]
    pub(crate) fn cached_paths_len(&self) -> usize {
        self.analyses.len()
    }

    #[cfg(test)]
    pub(crate) fn cached_reference_data_len(&self, path: &Path) -> Option<(usize, usize, bool)> {
        let key = navigation_cache_key(path);
        self.analyses
            .get(&key)
            .map(SymbolAnalysis::cached_reference_data_len)
    }

    #[must_use]
    pub fn references_for_project_file_with_overlay(
        &mut self,
        path: &Path,
        overlay_text: Option<&str>,
        line: usize,
        character: usize,
        include_declaration: bool,
    ) -> Vec<ToolLocation> {
        let Some(context) = self.analysis(path, overlay_text) else {
            return Vec::new();
        };
        let Some(binding_id) = context.binding_at(line, character) else {
            return Vec::new();
        };
        context.references(binding_id, include_declaration)
    }

    #[must_use]
    pub fn reference_lenses_for_project_file_with_overlay(
        &mut self,
        path: &Path,
        overlay_text: Option<&str>,
    ) -> Vec<ToolReferenceLens> {
        let Some(context) = self.analysis(path, overlay_text) else {
            return Vec::new();
        };
        context.reference_lenses()
    }

    fn analysis(&mut self, path: &Path, overlay_text: Option<&str>) -> Option<&mut SymbolAnalysis> {
        let key = navigation_cache_key(path);
        if self
            .analyses
            .get(&key)
            .is_none_or(|analysis| !analysis.overlay_matches(overlay_text))
        {
            let analysis = SymbolAnalysis::new(path, overlay_text)?;
            let _ = self.analyses.insert(key.clone(), analysis);
        }
        self.analyses.get_mut(&key)
    }
}

fn navigation_cache_key(path: &Path) -> PathBuf {
    path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
}
