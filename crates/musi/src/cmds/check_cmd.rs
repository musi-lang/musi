use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::PathBuf;
use std::process;

use clap::Args;
use musi_shared::{DiagnosticBag, Interner, SourceDb};

use crate::compiler::{
    collect_dep_paths, parse_file, print_diags_and_exit, read_file, resolve_import_path,
};

#[derive(Args)]
pub(crate) struct CheckArgs {
    /// Musi source file to type-check
    pub(crate) file: PathBuf,
}

pub(crate) fn run(args: CheckArgs) {
    let file_path_str = args.file.to_string_lossy();
    let file_path = file_path_str.as_ref();
    let src = read_file(file_path);

    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let mut diags = DiagnosticBag::new();

    let (file_id, user_module) =
        parse_file(file_path, &src, &mut interner, &mut source_db, &mut diags);

    if diags.has_errors() {
        print_diags_and_exit(&diags, &source_db);
    }

    let user_file_path = args.file.as_path();
    let mut queue: Vec<String> = collect_dep_paths(&user_module, &interner);
    let mut compiled: HashSet<String> = HashSet::new();
    let mut dep_modules = Vec::new();
    let mut qi = 0;

    while qi < queue.len() {
        let import_path = queue[qi].clone();
        qi += 1;

        let dep_src_owned: String;
        let key: String;
        if let Some(native_src) = musi_native::source_for(&import_path) {
            key = import_path.clone();
            if !compiled.insert(key.clone()) {
                continue;
            }
            dep_src_owned = native_src.to_owned();
        } else {
            let full_path = resolve_import_path(&import_path, user_file_path);
            key = full_path.to_string_lossy().into_owned();
            if !compiled.insert(key.clone()) {
                continue;
            }
            dep_src_owned = match fs::read_to_string(&full_path) {
                Ok(s) => s,
                Err(_) => continue, // tolerate missing std files in check mode
            };
        }

        let (dep_file_id, dep_module) = parse_file(
            &key,
            &dep_src_owned,
            &mut interner,
            &mut source_db,
            &mut diags,
        );
        for path in collect_dep_paths(&dep_module, &interner) {
            queue.push(path);
        }
        dep_modules.push((import_path, dep_module, dep_file_id));
    }

    let mut import_map: HashMap<String, musi_sema::ModuleExports> = HashMap::new();
    let empty_imports = HashMap::new();
    for (path, dep_module, dep_file_id) in &dep_modules {
        let dep_result = musi_sema::analyze(
            dep_module,
            &interner,
            *dep_file_id,
            &mut diags,
            &empty_imports,
        );
        let exports = musi_sema::exports_of(&dep_result, dep_module, &interner);
        let _prev = import_map.insert(path.clone(), exports);
    }

    let _result = musi_sema::analyze(&user_module, &interner, file_id, &mut diags, &import_map);

    if diags.has_errors() {
        for diag in diags.iter() {
            eprintln!("{}", diag.render_simple(&source_db));
        }
        process::exit(1);
    }

    process::exit(0);
}
