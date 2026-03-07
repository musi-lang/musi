use std::collections::HashMap;
use std::path::PathBuf;
use std::process;

use clap::Args;
use musi_shared::{DiagnosticBag, Interner, SourceDb};

use crate::compiler::{collect_and_parse_deps, parse_file, print_diags_and_exit, read_file};

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
    let dep_modules = collect_and_parse_deps(
        &user_module,
        user_file_path,
        &mut interner,
        &mut source_db,
        &mut diags,
        true,
    );

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
        use std::io::IsTerminal;
        let use_color = std::io::stderr().is_terminal();
        for diag in diags.iter() {
            eprintln!("{}", diag.render_rich(&source_db, use_color));
        }
        process::exit(1);
    }

    println!("\u{2713} no type errors");
}
