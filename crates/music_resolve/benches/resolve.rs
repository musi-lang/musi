use std::path::PathBuf;

use criterion::{criterion_group, criterion_main, Criterion};
use music_db::Db;
use music_hir::lower;
use music_lex::Lexer;
use music_parse::parse;
use music_resolve::queries::ResolveDb;
use music_shared::{Interner, SourceMap};

fn build_resolve_db(source: &str) -> ResolveDb {
    let mut interner = Interner::new();
    let (tokens, _) = Lexer::new(source).lex();
    let (mut ast, _) = parse(&tokens, source, &mut interner);
    lower(&mut ast);
    let db = Db::new(ast, interner, SourceMap::default());
    let mut rdb = ResolveDb::new(db, PathBuf::new());
    rdb.seed_builtins();
    rdb
}

fn gen_bindings(count: usize) -> String {
    let mut src = String::new();
    for i in 0..count {
        src.push_str(&format!("let _v{i} := {i};\n"));
    }
    // Generate references to earlier bindings
    for i in 1..count {
        let prev = i - 1;
        src.push_str(&format!("let _r{i} := _v{prev} + _v{i};\n"));
    }
    src
}

fn bench_resolve_small(c: &mut Criterion) {
    let src = gen_bindings(20);
    let _ = c.bench_function("resolve_small", |b| {
        b.iter_batched(
            || build_resolve_db(&src),
            |mut rdb| rdb.resolve_module(),
            criterion::BatchSize::SmallInput,
        );
    });
}

fn bench_resolve_medium(c: &mut Criterion) {
    let src = gen_bindings(100);
    let _ = c.bench_function("resolve_medium", |b| {
        b.iter_batched(
            || build_resolve_db(&src),
            |mut rdb| rdb.resolve_module(),
            criterion::BatchSize::SmallInput,
        );
    });
}

fn bench_resolve_large(c: &mut Criterion) {
    let src = gen_bindings(500);
    let _ = c.bench_function("resolve_large", |b| {
        b.iter_batched(
            || build_resolve_db(&src),
            |mut rdb| rdb.resolve_module(),
            criterion::BatchSize::SmallInput,
        );
    });
}

criterion_group!(
    resolve_benches,
    bench_resolve_small,
    bench_resolve_medium,
    bench_resolve_large
);
criterion_main!(resolve_benches);
