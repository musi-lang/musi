use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use music_config::{Workspace, load_config};
use music_db::Db;
use music_hir::lower;
use music_lex::Lexer;
use music_parse::parse;
use music_resolve::queries::{ResolutionMap, ResolveDb};
use music_resolve::{ModuleId, ModuleLoader, ProjectResolution, resolve_project};
use music_sema::{TypeEnv, type_check};
use music_shared::diag::Diag;
use music_shared::diag::emit_to_stderr;
use music_shared::{Interner, SourceMap};

pub struct CompileResult {
    pub db: Db,
    pub resolution: ResolutionMap,
    pub type_env: TypeEnv,
    pub diagnostics: Vec<Diag>,
    pub has_errors: bool,
}

pub struct ProjectModuleCheck {
    pub module_id: ModuleId,
    pub has_errors: bool,
}

pub struct ProjectCheckResult {
    pub loader: ModuleLoader,
    pub project: ProjectResolution,
    pub modules: Vec<ProjectModuleCheck>,
    pub has_errors: bool,
}

/// # Errors
///
/// Returns `Err` if the source file cannot be read (e.g. not found, permission denied).
pub fn compile(path: &Path) -> Result<CompileResult, io::Error> {
    let source = fs::read_to_string(path)?;

    let mut source_map = SourceMap::default();
    let source_id = source_map.add(path, source.as_str());

    let mut interner = Interner::new();

    let (tokens, lex_errors) = Lexer::new(&source).lex();
    let (mut ast, parse_errors) = parse(&tokens, &source, &mut interner);
    lower(&mut ast);

    let db = Db::new(ast, interner, source_map);

    let root = path
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .to_path_buf();
    let mut rdb = ResolveDb::new(db, root);
    rdb.set_current_file(path.to_path_buf());
    rdb.seed_builtins();
    rdb.resolve_module();
    let (db, resolution, resolve_errors) = rdb.finish();

    let (db, resolution, type_env, sema_errors) = type_check(db, resolution, None);

    let has_errors = !lex_errors.is_empty()
        || !parse_errors.is_empty()
        || !resolve_errors.is_empty()
        || !sema_errors.is_empty();

    let mut diagnostics: Vec<Diag> = Vec::new();
    for e in lex_errors {
        diagnostics.push(Diag::error(e.to_string()).with_label(e.span, source_id, ""));
    }
    for e in parse_errors {
        diagnostics.push(Diag::error(e.to_string()).with_label(e.span, source_id, ""));
    }
    for e in resolve_errors {
        diagnostics.push(Diag::error(e.to_string()).with_label(e.span, source_id, ""));
    }
    for e in sema_errors {
        diagnostics.push(Diag::error(e.to_string()).with_label(e.span, source_id, ""));
    }

    Ok(CompileResult {
        db,
        resolution,
        type_env,
        diagnostics,
        has_errors,
    })
}

/// Resolve and type-check the full project reachable from `path`.
///
/// # Errors
///
/// Returns `Err` if project config cannot be loaded, the entrypoint cannot be
/// read, or project resolution fails structurally.
pub fn analyze_project(path: &Path) -> Result<ProjectCheckResult, io::Error> {
    let (project_root, imports) = load_project_loader_config(path)?;
    let loader = ModuleLoader::new(project_root).with_config_imports(imports);
    let mut project =
        resolve_project(path, &loader).map_err(|e| io::Error::other(e.to_string()))?;
    let order = project.order.clone();
    let mut modules = Vec::with_capacity(order.len());
    let mut has_errors = false;

    for mod_id in order {
        let module = project
            .modules
            .remove(&mod_id)
            .ok_or_else(|| io::Error::other("module missing from project resolution"))?;
        let mut diagnostics = module.diagnostics;
        let source_id = module.db.source.iter().next().map(|source| source.id());

        let (db, resolution, module_has_errors) = if module.has_errors {
            (module.db, module.resolution, true)
        } else {
            let mut db = module.db;
            lower(&mut db.ast);
            let (db, resolution, _type_env, sema_errors) =
                type_check(db, module.resolution, None);
            if let Some(source_id) = source_id {
                for err in &sema_errors {
                    diagnostics
                        .push(Diag::error(err.to_string()).with_label(err.span, source_id, ""));
                }
            }
            (db, resolution, !sema_errors.is_empty())
        };

        has_errors |= module_has_errors;
        let _prev = project.modules.insert(
            mod_id,
            music_resolve::ModuleResult {
                db,
                resolution,
                errors: module.errors,
                diagnostics,
                has_errors: module_has_errors,
            },
        );
        modules.push(ProjectModuleCheck {
            module_id: mod_id,
            has_errors: module_has_errors,
        });
    }

    Ok(ProjectCheckResult {
        loader,
        project,
        modules,
        has_errors,
    })
}

pub fn compile_project(path: &Path) -> Result<ProjectCheckResult, io::Error> {
    analyze_project(path)
}

pub fn emit_project_diagnostics(result: &ProjectCheckResult) {
    for module in &result.modules {
        if let Some(data) = result.project.modules.get(&module.module_id) {
            for diag in &data.diagnostics {
                emit_to_stderr(diag, &data.db.source);
            }
        }
    }
}

fn load_project_loader_config(entry: &Path) -> Result<(PathBuf, HashMap<String, String>), io::Error> {
    let configs = find_project_configs(entry);
    let root = select_loader_root(&configs).unwrap_or_else(|| {
        entry
            .parent()
            .unwrap_or_else(|| Path::new("."))
            .to_path_buf()
    });
    let mut imports = HashMap::new();

    for (config_dir, config) in configs.iter().rev() {
        collect_local_package_imports(&root, config_dir, config, &mut imports)?;
        if let Some(config_imports) = &config.imports {
            imports.extend(config_imports.clone());
        }
        if let Some(overrides) = &config.overrides {
            imports.extend(overrides.clone());
        }
    }

    Ok((root, imports))
}

fn collect_local_package_imports(
    root: &Path,
    config_dir: &Path,
    config: &music_config::MusiConfig,
    imports: &mut HashMap<String, String>,
) -> Result<(), io::Error> {
    if let Some(name) = &config.name {
        let _ = imports
            .entry(name.clone())
            .or_insert_with(|| path_from_root(root, config_dir));
    }

    let Some(workspace_members) = workspace_members(config) else {
        return Ok(());
    };

    for member in workspace_members {
        let member_dir = root.join(member);
        let member_config_path = member_dir.join("musi.json");
        if !member_config_path.exists() {
            continue;
        }

        let member_config = load_config(&member_config_path)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;
        let Some(name) = member_config.name else {
            continue;
        };
        let member_path = path_from_root(root, &member_dir);
        let _ = imports.entry(name).or_insert(member_path);
    }

    Ok(())
}

fn workspace_members(config: &music_config::MusiConfig) -> Option<&[String]> {
    match config.workspace.as_ref()? {
        Workspace::Members(members) => Some(members.as_slice()),
        Workspace::Object(obj) => obj.members.as_deref(),
    }
}

fn find_project_configs(entry: &Path) -> Vec<(PathBuf, music_config::MusiConfig)> {
    let start = if entry.is_dir() {
        entry
    } else {
        entry.parent().unwrap_or_else(|| Path::new("."))
    };

    start
        .ancestors()
        .filter_map(|dir| {
            let config_path = dir.join("musi.json");
            if !config_path.exists() {
                return None;
            }
            let config = load_config(&config_path).ok()?;
            Some((dir.to_path_buf(), config))
        })
        .collect()
}

fn select_loader_root(configs: &[(PathBuf, music_config::MusiConfig)]) -> Option<PathBuf> {
    configs
        .iter()
        .rev()
        .find(|(_, config)| config.workspace.is_some())
        .map(|(path, _)| path.clone())
        .or_else(|| configs.first().map(|(path, _)| path.clone()))
}

fn path_from_root(root: &Path, target: &Path) -> String {
    if root == target {
        return "./".to_owned();
    }

    target
        .strip_prefix(root)
        .ok()
        .map(|path| format!("./{}", path.display()))
        .unwrap_or_else(|| target.display().to_string())
}
