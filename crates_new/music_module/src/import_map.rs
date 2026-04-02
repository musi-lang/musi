use std::collections::BTreeMap;

use crate::{ModuleKey, ModuleSpecifier};

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ImportMap {
    pub imports: BTreeMap<String, String>,
    pub scopes: BTreeMap<String, BTreeMap<String, String>>,
}

impl ImportMap {
    #[must_use]
    pub fn resolve(&self, from: &ModuleKey, spec: &ModuleSpecifier) -> Option<ModuleSpecifier> {
        let from = from.as_str();
        let spec = spec.as_str();

        if let Some(scoped) = longest_scope_prefix(&self.scopes, from) {
            if let Some(resolved) = resolve_in_map(scoped, spec) {
                return Some(ModuleSpecifier::new(resolved));
            }
        }

        resolve_in_map(&self.imports, spec).map(ModuleSpecifier::new)
    }
}

fn longest_scope_prefix<'a>(
    scopes: &'a BTreeMap<String, BTreeMap<String, String>>,
    from: &str,
) -> Option<&'a BTreeMap<String, String>> {
    let mut best: Option<&'a BTreeMap<String, String>> = None;
    let mut best_len: usize = 0;
    for (key, map) in scopes {
        if from.starts_with(key) && key.len() >= best_len {
            best = Some(map);
            best_len = key.len();
        }
    }
    best
}

fn resolve_in_map(map: &BTreeMap<String, String>, spec: &str) -> Option<String> {
    if let Some(target) = map.get(spec) {
        return Some(target.clone());
    }

    let mut best_key: Option<&str> = None;
    let mut best_len: usize = 0;
    for key in map.keys().map(String::as_str) {
        if !key.ends_with('/') {
            continue;
        }
        if spec.starts_with(key) && key.len() >= best_len {
            best_key = Some(key);
            best_len = key.len();
        }
    }
    let prefix = best_key?;
    let target = map.get(prefix)?;
    let rest = spec.strip_prefix(prefix).unwrap_or("");
    Some(format!("{target}{rest}"))
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
