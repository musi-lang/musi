use std::collections::{BTreeMap, BTreeSet};
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};

use musi_foundation::{extend_import_map, register_modules, resolve_spec};
use music_base::SourceId;
use music_module::{ImportMap, ImportSiteKind, ModuleKey, collect_import_sites};
use music_session::{Session, SessionOptions};
use music_syntax::{Lexer, parse};

use crate::{ToolingError, ToolingResult};

type DirectModuleMap = BTreeMap<ModuleKey, DirectModule>;
type DirectScopeMap = BTreeMap<String, String>;

#[derive(Debug, Clone)]
pub struct DirectGraph {
    entry_key: ModuleKey,
    texts: BTreeMap<ModuleKey, String>,
    import_map: ImportMap,
}

#[derive(Debug, Clone)]
struct DirectModule {
    key: ModuleKey,
    path: PathBuf,
    text: String,
    imports: Vec<String>,
}

impl DirectGraph {
    #[must_use]
    pub const fn entry_key(&self) -> &ModuleKey {
        &self.entry_key
    }

    pub fn module_texts(&self) -> impl Iterator<Item = (&ModuleKey, &str)> {
        self.texts.iter().map(|(key, text)| (key, text.as_str()))
    }

    /// # Errors
    ///
    /// Returns [`ToolingError`] when foundation or direct graph modules cannot be loaded into the
    /// session.
    pub fn build_session(&self, options: SessionOptions) -> ToolingResult<Session> {
        let mut import_map = self.import_map.clone();
        extend_import_map(&mut import_map);
        let mut session = Session::new(SessionOptions {
            emit: options.emit,
            import_map,
            target: options.target,
        });
        register_modules(&mut session).map_err(ToolingError::from)?;
        for (key, text) in &self.texts {
            session
                .set_module_text(key, text.clone())
                .map_err(ToolingError::from)?;
        }
        Ok(session)
    }
}

/// # Errors
///
/// Returns [`ToolingError`] when `entry_path` or any reachable direct import cannot be read.
pub fn load_direct_graph(entry_path: &Path) -> ToolingResult<DirectGraph> {
    let entry_path = canonical_source_path(entry_path)?;
    let mut seen = BTreeSet::new();
    let mut modules = DirectModuleMap::new();
    load_module_recursive(&entry_path, &mut seen, &mut modules)?;
    let import_map = build_import_map(&modules)?;
    let texts = modules
        .values()
        .map(|module| (module.key.clone(), module.text.clone()))
        .collect::<BTreeMap<_, _>>();
    Ok(DirectGraph {
        entry_key: module_key_for_path(&entry_path),
        texts,
        import_map,
    })
}

fn load_module_recursive(
    path: &Path,
    seen: &mut BTreeSet<PathBuf>,
    modules: &mut DirectModuleMap,
) -> ToolingResult {
    if !seen.insert(path.to_path_buf()) {
        return Ok(());
    }
    let text = fs::read_to_string(path).map_err(|source| ToolingError::ToolingIoFailed {
        path: path.to_path_buf(),
        source,
    })?;
    let key = module_key_for_path(path);
    let imports = collect_static_imports(&text);
    let module = DirectModule {
        key: key.clone(),
        path: path.to_path_buf(),
        text,
        imports: imports.clone(),
    };
    let _ = modules.insert(key, module);
    for spec in imports {
        if resolve_spec(spec.as_str()).is_some() {
            continue;
        }
        let target = resolve_relative_import(path, spec.as_str())?;
        load_module_recursive(&target, seen, modules)?;
    }
    Ok(())
}

fn collect_static_imports(text: &str) -> Vec<String> {
    let parsed = parse(Lexer::new(text).lex());
    collect_import_sites(SourceId::from_raw(0), parsed.tree())
        .into_iter()
        .filter_map(|site| match site.kind {
            ImportSiteKind::Static { spec } => Some(spec.as_str().to_owned()),
            ImportSiteKind::Dynamic | ImportSiteKind::InvalidStringLit => None,
        })
        .collect()
}

fn build_import_map(modules: &DirectModuleMap) -> ToolingResult<ImportMap> {
    let mut import_map = ImportMap::default();
    for module in modules.values() {
        let mut scope = DirectScopeMap::new();
        for spec in &module.imports {
            if let Some(target) = resolve_spec(spec.as_str()) {
                let _ = scope.insert(spec.clone(), target.as_str().to_owned());
                continue;
            }
            let target = resolve_relative_import(&module.path, spec)?;
            let _ = scope.insert(
                spec.clone(),
                module_key_for_path(&target).as_str().to_owned(),
            );
        }
        if !scope.is_empty() {
            let _ = import_map
                .scopes
                .insert(module.key.as_str().to_owned(), scope);
        }
    }
    Ok(import_map)
}

fn resolve_relative_import(from_path: &Path, spec: &str) -> ToolingResult<PathBuf> {
    if !is_relative_spec(spec) {
        return Err(ToolingError::PackageImportRequiresMusi {
            spec: spec.to_owned(),
        });
    }
    let base_dir = from_path
        .parent()
        .ok_or_else(|| ToolingError::MissingImport {
            path: from_path.to_path_buf(),
            spec: spec.to_owned(),
        })?;
    let candidate = base_dir.join(spec);
    for path in candidate_paths(&candidate) {
        if path.is_file() {
            return canonical_source_path(&path);
        }
    }
    Err(ToolingError::MissingImport {
        path: from_path.to_path_buf(),
        spec: spec.to_owned(),
    })
}

fn is_relative_spec(spec: &str) -> bool {
    spec.starts_with("./") || spec.starts_with("../") || spec.starts_with('.')
}

fn candidate_paths(path: &Path) -> [PathBuf; 3] {
    [
        path.with_extension("ms"),
        path.join("index.ms"),
        path.to_path_buf(),
    ]
}

fn canonical_source_path(path: &Path) -> ToolingResult<PathBuf> {
    if path.extension() != Some(OsStr::new("ms")) && !path.is_dir() {
        let source_path = path.with_extension("ms");
        if source_path.is_file() {
            return source_path
                .canonicalize()
                .map_err(|source| ToolingError::ToolingIoFailed {
                    path: source_path,
                    source,
                });
        }
    }
    if !path.exists() {
        return Err(ToolingError::MissingEntrySource {
            path: path.to_path_buf(),
        });
    }
    path.canonicalize()
        .map_err(|source| ToolingError::ToolingIoFailed {
            path: path.to_path_buf(),
            source,
        })
}

fn module_key_for_path(path: &Path) -> ModuleKey {
    ModuleKey::new(path.display().to_string())
}
