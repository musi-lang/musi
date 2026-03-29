use criterion::{Criterion, criterion_group, criterion_main};
use music_lex::Lexer;

fn bench_identifiers(c: &mut Criterion) {
    let source = "foo bar baz qux alpha beta gamma delta ".repeat(200);
    let _ = c.bench_function("lex_identifiers", |b| b.iter(|| Lexer::new(&source).lex()));
}

fn bench_numbers(c: &mut Criterion) {
    let source = "42 0xFF 3.14 1_000_000 0b1010 0o777 ".repeat(200);
    let _ = c.bench_function("lex_numbers", |b| b.iter(|| Lexer::new(&source).lex()));
}

fn bench_operators(c: &mut Criterion) {
    let source = "+ - * / = /= < > <= >= := <- -> ~> |> :? :?> ... ".repeat(100);
    let _ = c.bench_function("lex_operators", |b| b.iter(|| Lexer::new(&source).lex()));
}

fn bench_mixed(c: &mut Criterion) {
    let source = r#"
let Point := data { x : Float; y : Float };
let origin := .{ x := 0.0, y := 0.0 };
let render := (name) => f"Point {name}";
case origin of (
| value if value.x >= 0.0 => value
| _ => origin
);
"#
    .repeat(80);
    let _ = c.bench_function("lex_mixed", |b| b.iter(|| Lexer::new(&source).lex()));
}

criterion_group!(
    lex_benches,
    bench_identifiers,
    bench_numbers,
    bench_operators,
    bench_mixed
);
criterion_main!(lex_benches);
