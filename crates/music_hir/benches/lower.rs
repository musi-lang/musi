use criterion::{criterion_group, criterion_main, Criterion};
use music_lex::Lexer;
use music_parse::parse;
use music_shared::Interner;

use music_hir::lower;

fn parse_source(source: &str) -> (music_ast::data::AstData, Interner) {
    let mut interner = Interner::new();
    let (tokens, _) = Lexer::new(source).lex();
    let (ast, _) = parse(&tokens, source, &mut interner);
    (ast, interner)
}

fn bench_lower_piecewise(c: &mut Criterion) {
    let src = "(1 if x > 0 | 2 if x > 10 | 0 if _);\n".repeat(200);
    let _ = c.bench_function("lower_piecewise", |b| {
        b.iter_batched(
            || parse_source(&src).0,
            |mut ast| lower(&mut ast),
            criterion::BatchSize::SmallInput,
        );
    });
}

fn bench_lower_pipes(c: &mut Criterion) {
    let src = "a |> f |> g |> h |> k;\n".repeat(200);
    let _ = c.bench_function("lower_pipes", |b| {
        b.iter_batched(
            || parse_source(&src).0,
            |mut ast| lower(&mut ast),
            criterion::BatchSize::SmallInput,
        );
    });
}

fn bench_lower_mixed(c: &mut Criterion) {
    let src = r#"
let x := 42;
let y := (1 if x > 0 | 0 if _);
let z := x |> f |> g;
let w := (10 if z > 5 | z if _);
let r := w |> h |> k;
"#
    .repeat(100);
    let _ = c.bench_function("lower_mixed", |b| {
        b.iter_batched(
            || parse_source(&src).0,
            |mut ast| lower(&mut ast),
            criterion::BatchSize::SmallInput,
        );
    });
}

criterion_group!(
    lower_benches,
    bench_lower_piecewise,
    bench_lower_pipes,
    bench_lower_mixed
);
criterion_main!(lower_benches);
