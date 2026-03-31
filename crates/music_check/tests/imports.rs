use std::collections::HashMap;

use music_basic::SourceId;
use music_basic::SourceMap;
use music_check::iface::{ModuleExportSummary, SemaImportEnv};
use music_known::KnownSymbols;
use music_lex::Lexer;
use music_names::Interner;
use music_resolve::{ImportEnv, ResolveOptions};

struct TestImportEnv {
    modules: HashMap<String, ModuleExportSummary>,
    exports: HashMap<String, Box<[String]>>,
}

impl TestImportEnv {
    fn new() -> Self {
        Self {
            modules: HashMap::new(),
            exports: HashMap::new(),
        }
    }

    fn insert(&mut self, path: impl Into<String>, summary: ModuleExportSummary) {
        let path = path.into();
        let exports = summary
            .exports
            .iter()
            .map(|e| e.name.clone())
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let _prev = self.exports.insert(path.clone(), exports);
        let _prev = self.modules.insert(path, summary);
    }
}

impl ImportEnv for TestImportEnv {
    fn has_module(&self, _from: SourceId, path: &str) -> bool {
        self.modules.contains_key(path)
    }

    fn for_each_export(&self, _from: SourceId, path: &str, f: &mut dyn FnMut(&str)) {
        let Some(exports) = self.exports.get(path) else {
            return;
        };
        for e in exports.iter() {
            f(e.as_str());
        }
    }

    fn is_export_opaque(&self, _from: SourceId, path: &str, name: &str) -> bool {
        self.modules
            .get(path)
            .is_some_and(|m| m.is_export_opaque(name))
    }
}

impl SemaImportEnv for TestImportEnv {
    fn module_summary(&self, _from: SourceId, path: &str) -> Option<&ModuleExportSummary> {
        self.modules.get(path)
    }
}

#[test]
fn test_statement_import_binds_effect_family_from_summary() {
    let mut sources = SourceMap::new();
    let dep_path = "dep.ms";
    let dep_id = sources.add(
        dep_path,
        r#"
export let Console := effect { let writeln (msg : String) : Unit; };
"#,
    );
    let entry_path = "entry.ms";
    let entry_id = sources.add(
        entry_path,
        r#"
import "./dep.ms";
let f () with { Console } : Unit := perform Console.writeln("x");
f();
"#,
    );

    let mut interner = Interner::new();
    let known = KnownSymbols::new(&mut interner);

    let dep_src = sources.get(dep_id).expect("source");
    let dep_lexed = Lexer::new(dep_src.text()).lex();
    let dep_parsed = music_parse::parse(dep_id, &dep_lexed);
    assert!(dep_parsed.errors().is_empty());
    let dep_analyzed = music_check::analyze_module(
        dep_parsed.tree(),
        &sources,
        &mut interner,
        ResolveOptions {
            prelude: known.compiler_prelude().to_vec(),
            import_env: None,
        },
        None,
    );
    assert!(dep_analyzed.resolve_errors.is_empty());
    assert!(dep_analyzed.check_errors.is_empty());

    let mut env = TestImportEnv::new();
    env.insert(dep_path, dep_analyzed.interface.clone());

    let entry_src = sources.get(entry_id).expect("source");
    let entry_lexed = Lexer::new(entry_src.text()).lex();
    let entry_parsed = music_parse::parse(entry_id, &entry_lexed);
    assert!(entry_parsed.errors().is_empty());

    let entry_analyzed = music_check::analyze_module(
        entry_parsed.tree(),
        &sources,
        &mut interner,
        ResolveOptions {
            prelude: known.compiler_prelude().to_vec(),
            import_env: Some(&env),
        },
        Some(&env),
    );

    assert!(entry_analyzed.resolve_errors.is_empty());
    assert!(
        entry_analyzed.check_errors.is_empty(),
        "expected no errors, got {:?}",
        entry_analyzed
            .check_errors
            .iter()
            .map(|e| &e.kind)
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_record_pattern_import_binds_effect_family_from_summary() {
    let mut sources = SourceMap::new();
    let dep_path = "dep.ms";
    let dep_id = sources.add(
        dep_path,
        r#"
export let Console := effect { let writeln (msg : String) : Unit; };
"#,
    );
    let entry_path = "entry.ms";
    let entry_id = sources.add(
        entry_path,
        r#"
let { Console } := import "./dep.ms";
let f () with { Console } : Unit := perform Console.writeln("x");
f();
"#,
    );

    let mut interner = Interner::new();
    let known = KnownSymbols::new(&mut interner);

    let dep_src = sources.get(dep_id).expect("source");
    let dep_lexed = Lexer::new(dep_src.text()).lex();
    let dep_parsed = music_parse::parse(dep_id, &dep_lexed);
    assert!(dep_parsed.errors().is_empty());
    let dep_analyzed = music_check::analyze_module(
        dep_parsed.tree(),
        &sources,
        &mut interner,
        ResolveOptions {
            prelude: known.compiler_prelude().to_vec(),
            import_env: None,
        },
        None,
    );
    assert!(dep_analyzed.resolve_errors.is_empty());
    assert!(dep_analyzed.check_errors.is_empty());

    let mut env = TestImportEnv::new();
    env.insert(dep_path, dep_analyzed.interface.clone());

    let entry_src = sources.get(entry_id).expect("source");
    let entry_lexed = Lexer::new(entry_src.text()).lex();
    let entry_parsed = music_parse::parse(entry_id, &entry_lexed);
    assert!(entry_parsed.errors().is_empty());

    let entry_analyzed = music_check::analyze_module(
        entry_parsed.tree(),
        &sources,
        &mut interner,
        ResolveOptions {
            prelude: known.compiler_prelude().to_vec(),
            import_env: Some(&env),
        },
        Some(&env),
    );

    assert!(entry_analyzed.resolve_errors.is_empty());
    assert!(
        entry_analyzed.check_errors.is_empty(),
        "expected no errors, got {:?}",
        entry_analyzed
            .check_errors
            .iter()
            .map(|e| &e.kind)
            .collect::<Vec<_>>()
    );
}
