use std::collections::BTreeMap;
use std::ffi::OsStr;
use std::fs;
use std::path::{Component, Path, PathBuf};

use musi_foundation::resolve_spec;
use music_base::SourceId;
use music_base::diag::{Diag, DiagContext, OwnedSourceDiag};
use music_module::{ImportMap, ImportSiteKind, ModuleKey, ModuleSpecifier, collect_import_sites};
use music_syntax::{Lexer, parse};

use crate::errors::ProjectError;
use crate::lock::{LockedPackage, LockedPackageSource, Lockfile};
use crate::manifest::CompilerOptions;
use crate::project::model::{
    DependencyPackageMap, LoadedImportSite, LoadedModule, PackageId, PackageRecord, PackageSource,
};
use crate::{ProjectDiagKind, ProjectResult};

type PackageRecordMap = BTreeMap<PackageId, PackageRecord>;
type RelativeModuleMap = BTreeMap<String, ModuleKey>;

pub(super) fn discover_modules(
    package_id: &PackageId,
    root_dir: &Path,
) -> ProjectResult<BTreeMap<ModuleKey, LoadedModule>> {
    let mut out = BTreeMap::new();
    discover_modules_recursive(package_id, root_dir, root_dir, &mut out)?;
    Ok(out)
}

fn discover_modules_recursive(
    package_id: &PackageId,
    root_dir: &Path,
    dir: &Path,
    out: &mut BTreeMap<ModuleKey, LoadedModule>,
) -> ProjectResult {
    let entries = fs::read_dir(dir).map_err(|source| ProjectError::ProjectIoFailed {
        path: dir.to_path_buf(),
        source,
    })?;
    for entry in entries {
        let entry = entry.map_err(|source| ProjectError::ProjectIoFailed {
            path: dir.to_path_buf(),
            source,
        })?;
        let path = entry.path();
        if path.is_dir() {
            discover_modules_recursive(package_id, root_dir, &path, out)?;
            continue;
        }
        if path.extension() != Some(OsStr::new("ms")) {
            continue;
        }
        let text = fs::read_to_string(&path).map_err(|source| ProjectError::ProjectIoFailed {
            path: path.clone(),
            source,
        })?;
        let relative = normalize_relative_path(
            path.strip_prefix(root_dir)
                .map_err(|_| ProjectError::MissingModule { path: path.clone() })?,
        );
        let key = module_key_for(package_id, &relative);
        let scope_key = ModuleKey::new(format!("./{relative}"));
        let parsed = parse(Lexer::new(&text).lex());
        let imports = collect_import_sites(SourceId::from_raw(0), parsed.tree())
            .into_iter()
            .filter_map(|site| match site.kind {
                ImportSiteKind::Static { spec } => Some(LoadedImportSite {
                    spec: spec.as_str().to_owned(),
                    span: site.span,
                }),
                ImportSiteKind::NonLiteral | ImportSiteKind::InvalidStringLit => None,
            })
            .collect::<Vec<_>>();
        let module_path = normalize_lookup_path(&path);
        let module = LoadedModule {
            key: key.clone(),
            path: module_path,
            package_relative: relative,
            local_scope_key: scope_key,
            text,
            imports,
        };
        let _ = out.insert(key, module);
    }
    Ok(())
}

pub(super) fn build_import_map(
    package_records: &PackageRecordMap,
    package_name_index: &DependencyPackageMap,
) -> ProjectResult<ImportMap> {
    let mut import_map = ImportMap::default();
    for record in package_records.values() {
        let manifest_map = ImportMap {
            imports: record.package.manifest.imports.clone(),
            scopes: record.package.manifest.scopes.clone(),
        };
        for module in record.modules.values() {
            let scope = import_map
                .scopes
                .entry(module.key.as_str().to_owned())
                .or_default();
            for import_site in &module.imports {
                let import_spec = import_site.spec.as_str();
                let remapped = manifest_map
                    .resolve(&module.local_scope_key, &ModuleSpecifier::new(import_spec));
                let remapped_spec = remapped
                    .as_ref()
                    .map_or(import_spec, ModuleSpecifier::as_str);
                let target = if remapped.is_some() {
                    resolve_import_map_target(
                        record,
                        module,
                        remapped_spec,
                        package_records,
                        package_name_index,
                    )
                } else {
                    resolve_import_spec(
                        record,
                        module,
                        import_spec,
                        package_records,
                        package_name_index,
                    )
                }
                .ok_or_else(|| unresolved_import(module, import_site, remapped_spec))?;
                let _ = scope.insert(import_spec.to_owned(), target.as_str().to_owned());
            }
        }
    }
    Ok(import_map)
}

fn unresolved_import(
    module: &LoadedModule,
    import_site: &LoadedImportSite,
    remapped: &str,
) -> ProjectError {
    let context = DiagContext::new().with("spec", &import_site.spec);
    let mut diag = Diag::error(ProjectDiagKind::SourceImportUnresolved.message_with(&context))
        .with_code(ProjectDiagKind::SourceImportUnresolved.code())
        .with_label(
            import_site.span,
            SourceId::from_raw(0),
            ProjectDiagKind::SourceImportUnresolved.label_with(&context),
        );
    if remapped != import_site.spec {
        diag = diag.with_note(format!(
            "import map resolved `{}` to `{remapped}`",
            import_site.spec
        ));
    }
    if let Some(hint) = ProjectDiagKind::SourceImportUnresolved.hint() {
        diag = diag.with_hint(hint);
    }
    ProjectError::SourceDiagnostic(Box::new(OwnedSourceDiag::new(
        module.path.clone(),
        module.text.clone(),
        diag,
    )))
}

fn resolve_import_map_target(
    package: &PackageRecord,
    module: &LoadedModule,
    spec: &str,
    package_records: &PackageRecordMap,
    package_name_index: &BTreeMap<String, PackageId>,
) -> Option<ModuleKey> {
    resolve_package_root_spec(package, spec)
        .or_else(|| resolve_import_spec(package, module, spec, package_records, package_name_index))
}

fn resolve_package_root_spec(package: &PackageRecord, spec: &str) -> Option<ModuleKey> {
    if let Some(target) = resolve_spec(spec) {
        return Some(target);
    }
    if spec.starts_with("./") || spec.starts_with("../") {
        return resolve_module_target(
            &package.package.root_dir,
            &package.relative_modules,
            None,
            spec,
        );
    }
    None
}

fn resolve_import_spec(
    package: &PackageRecord,
    module: &LoadedModule,
    spec: &str,
    package_records: &PackageRecordMap,
    package_name_index: &BTreeMap<String, PackageId>,
) -> Option<ModuleKey> {
    if let Some(target) = resolve_spec(spec) {
        return Some(target);
    }
    if let Some(target) = resolve_compiler_path(
        &package.package.root_dir,
        &package.relative_modules,
        package.package.manifest.compiler_options.as_ref(),
        spec,
    ) {
        return Some(target);
    }
    if let Some(target) = resolve_module_target(
        &package.package.root_dir,
        &package.relative_modules,
        Some(Path::new(&module.package_relative)),
        spec,
    ) {
        return Some(target);
    }

    let (package_name, export_key) = split_package_spec(spec);
    let target_id = package_name_index.get(package_name.as_str())?;
    let target_package = package_records.get(target_id)?;
    target_package
        .package
        .exports
        .get(export_key.as_str())
        .cloned()
}

fn resolve_compiler_path(
    root_dir: &Path,
    relative_modules: &RelativeModuleMap,
    compiler_options: Option<&CompilerOptions>,
    spec: &str,
) -> Option<ModuleKey> {
    let compiler_options = compiler_options?;

    if let Some(candidates) = compiler_options.paths.get(spec) {
        for candidate in candidates {
            if let Some(target) = resolve_module_target(root_dir, relative_modules, None, candidate)
            {
                return Some(target);
            }
        }
    }

    for (pattern, candidates) in &compiler_options.paths {
        let Some(pattern_prefix) = pattern.strip_suffix('*') else {
            continue;
        };
        let Some(rest) = spec.strip_prefix(pattern_prefix) else {
            continue;
        };
        for candidate in candidates {
            let candidate = candidate.replace('*', rest);
            if let Some(target) =
                resolve_module_target(root_dir, relative_modules, None, &candidate)
            {
                return Some(target);
            }
        }
    }

    if let Some(base_url) = compiler_options.base_url.as_deref() {
        let base = root_dir.join(base_url);
        let candidate = base.join(spec);
        if let Some(target) = resolve_candidate_path(relative_modules, root_dir, &candidate) {
            return Some(target);
        }
    }

    None
}

pub(super) fn resolve_module_target(
    root_dir: &Path,
    relative_modules: &BTreeMap<String, ModuleKey>,
    from_module: Option<&Path>,
    spec: &str,
) -> Option<ModuleKey> {
    let spec_path = Path::new(spec);
    if spec_path.is_absolute() {
        return None;
    }
    let base_dir = from_module
        .and_then(Path::parent)
        .map(Path::to_path_buf)
        .unwrap_or_default();
    let candidate = if spec.starts_with("./") || spec.starts_with("../") {
        root_dir.join(base_dir).join(spec_path)
    } else if spec.starts_with('/') {
        root_dir.join(
            normalize_spec_path(spec)
                .strip_prefix(Path::new("/"))
                .unwrap_or_else(|_| Path::new(spec)),
        )
    } else if spec.starts_with('.') {
        root_dir.join(base_dir).join(spec_path)
    } else {
        root_dir.join(normalize_spec_path(spec))
    };
    resolve_candidate_path(relative_modules, root_dir, &candidate)
}

fn resolve_candidate_path(
    relative_modules: &BTreeMap<String, ModuleKey>,
    root_dir: &Path,
    candidate: &Path,
) -> Option<ModuleKey> {
    for path in candidate_paths(candidate) {
        if let Ok(relative) = path.strip_prefix(root_dir) {
            let relative = normalize_relative_path(relative);
            if let Some(target) = relative_modules.get(&relative) {
                return Some(target.clone());
            }
        }
    }
    None
}

fn candidate_paths(path: &Path) -> Vec<PathBuf> {
    if path.extension() == Some(OsStr::new("ms")) {
        return vec![path.to_path_buf()];
    }
    vec![
        path.with_extension("ms"),
        path.join("index.ms"),
        path.to_path_buf(),
    ]
}

fn split_package_spec(spec: &str) -> (String, String) {
    match spec.split_once('/') {
        Some((name, subpath)) => (name.into(), format_export_key(subpath)),
        None => (spec.into(), ".".into()),
    }
}

fn format_export_key(subpath: &str) -> String {
    format!("./{subpath}")
}

pub(super) fn module_key_for(package_id: &PackageId, relative: &str) -> ModuleKey {
    ModuleKey::new(format!(
        "@{}@{}/{}",
        package_id.name, package_id.version, relative
    ))
}

fn normalize_relative_path(path: &Path) -> String {
    path.components()
        .filter_map(|component| match component {
            Component::Normal(part) => Some(part.to_string_lossy().into_owned()),
            _ => None,
        })
        .collect::<Vec<_>>()
        .join("/")
}

fn normalize_spec_path(spec: &str) -> PathBuf {
    let mut out = PathBuf::new();
    for component in Path::new(spec).components() {
        match component {
            Component::ParentDir => {
                let _ = out.pop();
            }
            Component::Normal(part) => out.push(part),
            Component::CurDir | Component::RootDir | Component::Prefix(_) => {}
        }
    }
    out
}

pub(super) fn normalize_lookup_path(path: &Path) -> PathBuf {
    path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
}

pub(super) fn build_lockfile(package_records: &BTreeMap<PackageId, PackageRecord>) -> Lockfile {
    let packages = package_records
        .values()
        .filter_map(|record| {
            let source = match &record.package.source {
                PackageSource::Workspace => LockedPackageSource::workspace(),
                PackageSource::Registry { registry_dir, .. } => {
                    LockedPackageSource::registry(registry_dir.display().to_string())
                }
                PackageSource::Git {
                    url,
                    reference,
                    commit,
                    ..
                } => LockedPackageSource::git(url.clone(), reference.clone(), commit.clone()),
                PackageSource::Builtin => return None,
            };
            Some(LockedPackage {
                name: record.package.id.name.clone(),
                version: record.package.id.version.clone(),
                source,
            })
        })
        .collect::<Vec<_>>();
    Lockfile {
        version: 1,
        packages,
    }
    .normalized()
}
