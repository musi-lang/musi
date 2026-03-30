use std::collections::HashMap;

use music_basic::SourceId;
use music_basic::SourceMap;
use music_il::ConstantEntry;
use music_il::Opcode;
use music_il::Operand;
use music_known::KnownSymbols;
use music_lex::Lexer;
use music_names::Interner;
use music_resolve::ImportEnv;
use music_resolve::ResolveOptions;

use crate::{EmitModule, EmitProgram};

struct TestImportEnv {
    exports_by_path: HashMap<String, Vec<String>>,
}

impl TestImportEnv {
    fn new() -> Self {
        Self {
            exports_by_path: HashMap::new(),
        }
    }

    fn insert(&mut self, path: impl Into<String>, exports: Vec<String>) {
        let _prev = self.exports_by_path.insert(path.into(), exports);
    }
}

impl ImportEnv for TestImportEnv {
    fn has_module(&self, _from: SourceId, path: &str) -> bool {
        self.exports_by_path.contains_key(path)
    }

    fn for_each_export(&self, _from: SourceId, path: &str, f: &mut dyn FnMut(&str)) {
        let Some(exports) = self.exports_by_path.get(path) else {
            return;
        };
        for e in exports.iter() {
            f(e.as_str());
        }
    }

    fn is_export_opaque(&self, _from: SourceId, _path: &str, _name: &str) -> bool {
        false
    }
}

#[test]
fn test_emit_single_module_decodes_numeric_separators() {
    let mut sources = SourceMap::new();
    let path = "entry.ms";
    let source_id = sources.add(path, "let x := 123_456; let y := 12_3.4_5e1;");
    let source = sources.get(source_id).expect("source exists");

    let lexed = Lexer::new(source.text()).lex();
    let parsed = music_parse::parse(source_id, &lexed);
    assert!(parsed.errors().is_empty());

    let mut interner = Interner::new();
    let known = KnownSymbols::new(&mut interner);
    let options = ResolveOptions {
        prelude: known.compiler_prelude().to_vec(),
        import_env: None,
    };
    let analyzed = music_check::analyze_module(parsed.tree(), &sources, &mut interner, options);
    assert!(analyzed.resolve_errors.is_empty());
    assert!(analyzed.check_errors.is_empty());

    let artifact = super::emit_single_module(path, &interner, &sources, &analyzed)
        .expect("emit")
        .artifact;

    let entries = artifact.constants.entries();
    assert!(entries.contains(&ConstantEntry::Int(123_456)));
    assert!(entries.contains(&ConstantEntry::Float(1234.5_f64.to_bits())));
}

#[test]
fn test_emit_record_fields_are_canonicalized_by_name() {
    let mut sources = SourceMap::new();
    let path = "entry.ms";
    let source_id = sources.add(path, "let r := { y := 1, x := 2 }; r.x;");
    let source = sources.get(source_id).expect("source exists");

    let lexed = Lexer::new(source.text()).lex();
    let parsed = music_parse::parse(source_id, &lexed);
    assert!(parsed.errors().is_empty());

    let mut interner = Interner::new();
    let known = KnownSymbols::new(&mut interner);
    let options = ResolveOptions {
        prelude: known.compiler_prelude().to_vec(),
        import_env: None,
    };
    let analyzed = music_check::analyze_module(parsed.tree(), &sources, &mut interner, options);
    assert!(analyzed.resolve_errors.is_empty());
    assert!(analyzed.check_errors.is_empty());

    let artifact = super::emit_single_module(path, &interner, &sources, &analyzed)
        .expect("emit")
        .artifact;

    assert!(artifact.types.iter().any(|t| t.key == "record(x,y)"));
    let entry = artifact.methods.first().expect("entry method exists");
    assert!(
        entry
            .instructions
            .iter()
            .any(|i| i.opcode == Opcode::DataGet && i.operand == Operand::U16(0))
    );
}

#[test]
fn test_emit_record_update_copies_unchanged_fields() {
    let mut sources = SourceMap::new();
    let path = "entry.ms";
    let source_id = sources.add(
        path,
        "let p := { x := 1, y := 2 }; let q := p.{ x := 3 }; q.y;",
    );
    let source = sources.get(source_id).expect("source exists");

    let lexed = Lexer::new(source.text()).lex();
    let parsed = music_parse::parse(source_id, &lexed);
    assert!(parsed.errors().is_empty());

    let mut interner = Interner::new();
    let known = KnownSymbols::new(&mut interner);
    let options = ResolveOptions {
        prelude: known.compiler_prelude().to_vec(),
        import_env: None,
    };
    let analyzed = music_check::analyze_module(parsed.tree(), &sources, &mut interner, options);
    assert!(analyzed.resolve_errors.is_empty());
    assert!(analyzed.check_errors.is_empty());

    let artifact = super::emit_single_module(path, &interner, &sources, &analyzed)
        .expect("emit")
        .artifact;

    let entry = artifact.methods.first().expect("entry method exists");
    assert!(
        entry
            .instructions
            .iter()
            .any(|i| { i.opcode == Opcode::DataGet && i.operand == Operand::U16(1) })
    );
}

#[test]
fn test_emit_import_expr_builds_export_record() {
    let mut sources = SourceMap::new();
    let dep_path = "dep.ms";
    let dep_source_id = sources.add(dep_path, "export let a := 1;");

    let entry_path = "entry.ms";
    let entry_source_id = sources.add(entry_path, "let m := import \"./dep.ms\"; m.a;");
    let dep_source = sources.get(dep_source_id).expect("source exists");
    let entry_source = sources.get(entry_source_id).expect("source exists");

    let mut interner = Interner::new();
    let known = KnownSymbols::new(&mut interner);

    let dep_lexed = Lexer::new(dep_source.text()).lex();
    let dep_parsed = music_parse::parse(dep_source_id, &dep_lexed);
    assert!(dep_parsed.errors().is_empty());
    let dep_options = ResolveOptions {
        prelude: known.compiler_prelude().to_vec(),
        import_env: None,
    };
    let dep_analyzed =
        music_check::analyze_module(dep_parsed.tree(), &sources, &mut interner, dep_options);
    assert!(dep_analyzed.resolve_errors.is_empty());
    assert!(dep_analyzed.check_errors.is_empty());

    let mut env = TestImportEnv::new();
    env.insert(
        dep_path,
        dep_analyzed
            .exports
            .iter()
            .copied()
            .map(|s| interner.resolve(s).to_string())
            .collect(),
    );

    let entry_lexed = Lexer::new(entry_source.text()).lex();
    let entry_parsed = music_parse::parse(entry_source_id, &entry_lexed);
    assert!(entry_parsed.errors().is_empty());
    let entry_options = ResolveOptions {
        prelude: known.compiler_prelude().to_vec(),
        import_env: Some(&env),
    };
    let entry_analyzed =
        music_check::analyze_module(entry_parsed.tree(), &sources, &mut interner, entry_options);
    assert!(entry_analyzed.resolve_errors.is_empty());
    assert!(entry_analyzed.check_errors.is_empty());

    let modules_in_order = Box::new([
        EmitModule {
            path: dep_path,
            analyzed: &dep_analyzed,
        },
        EmitModule {
            path: entry_path,
            analyzed: &entry_analyzed,
        },
    ]);

    let artifact = super::emit_program(EmitProgram {
        interner: &interner,
        sources: &sources,
        modules_in_order,
        entry_path,
    })
    .expect("emit")
    .artifact;

    let dep_a_idx = artifact
        .globals
        .iter()
        .position(|g| g.name == "dep.ms::a")
        .and_then(|i| u16::try_from(i).ok())
        .expect("dep a global");

    let entry = artifact.methods.first().expect("entry method exists");
    assert!(
        entry
            .instructions
            .iter()
            .any(|i| { i.opcode == Opcode::LdGlob && i.operand == Operand::U16(dep_a_idx) })
    );
    assert!(
        entry
            .instructions
            .iter()
            .any(|i| i.opcode == Opcode::DataNew)
    );
}
