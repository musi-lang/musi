use std::path::PathBuf;

use criterion::{Criterion, criterion_group, criterion_main};
use music_db::Db;
use music_hir::lower;
use music_lex::Lexer;
use music_parse::parse;
use music_resolve::{ResolutionMap, ResolveDb};
use music_shared::{Interner, SourceMap};

use music_sema::type_check;

fn resolve_source(source: &str) -> (Db, ResolutionMap) {
    let mut interner = Interner::new();
    let (tokens, _) = Lexer::new(source).lex();
    let (mut ast, _) = parse(&tokens, source, &mut interner);
    lower(&mut ast);
    let db = Db::new(ast, interner, SourceMap::default());
    let mut rdb = ResolveDb::new(db, PathBuf::new());
    rdb.seed_builtins();
    rdb.resolve_module();
    let (db, resolution, _) = rdb.finish();
    (db, resolution)
}

fn bench_check_arithmetic(c: &mut Criterion) {
    let mut src = String::new();
    for i in 0..200 {
        src.push_str(&format!("let _a{i} := {i} + {i} * 2 - 1;\n"));
    }
    let _ = c.bench_function("check_arithmetic", |b| {
        b.iter_batched(
            || resolve_source(&src),
            |(db, resolution)| type_check(db, resolution, None),
            criterion::BatchSize::SmallInput,
        );
    });
}

fn bench_check_functions(c: &mut Criterion) {
    let mut src = String::new();
    for i in 0..100 {
        src.push_str(&format!("let _f{i} := (x : Int) : Int => x + {i};\n"));
    }
    let _ = c.bench_function("check_functions", |b| {
        b.iter_batched(
            || resolve_source(&src),
            |(db, resolution)| type_check(db, resolution, None),
            criterion::BatchSize::SmallInput,
        );
    });
}

fn bench_check_data_types(c: &mut Criterion) {
    let mut src = String::new();
    for i in 0..50 {
        src.push_str(&format!(
            "let _T{i} := data {{ | A{i} : Int | B{i} | C{i} : Float }};\n"
        ));
        src.push_str(&format!("let _v{i} := .A{i}(42);\n"));
    }
    let _ = c.bench_function("check_data_types", |b| {
        b.iter_batched(
            || resolve_source(&src),
            |(db, resolution)| type_check(db, resolution, None),
            criterion::BatchSize::SmallInput,
        );
    });
}

fn bench_check_mixed(c: &mut Criterion) {
    let src = r#"
let _x := 42;
let _y := 3.14;
let _add := (a : Int, b : Int) : Int => a + b;
let _Point := data { ; x : Float ; y : Float };
let _Shape := data { | Circle : Float | Rect : Float | Empty };
let _r := _add(1, 2);
let _s := .Circle(5.0);
"#
    .repeat(30);
    let _ = c.bench_function("check_mixed", |b| {
        b.iter_batched(
            || resolve_source(&src),
            |(db, resolution)| type_check(db, resolution, None),
            criterion::BatchSize::SmallInput,
        );
    });
}

criterion_group!(
    check_benches,
    bench_check_arithmetic,
    bench_check_functions,
    bench_check_data_types,
    bench_check_mixed
);
criterion_main!(check_benches);
