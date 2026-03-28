use criterion::{criterion_group, criterion_main, Criterion};
use music_lex::Lexer;

fn bench_identifiers(c: &mut Criterion) {
    let src = "foo bar baz qux hello world alpha beta gamma delta ".repeat(100);
    let _ = c.bench_function("lex_identifiers", |b| b.iter(|| Lexer::new(&src).lex()));
}

fn bench_numbers(c: &mut Criterion) {
    let src = "42 0xFF 3.14 1_000_000 0b1010 0o777 123 456 789 0 ".repeat(100);
    let _ = c.bench_function("lex_numbers", |b| b.iter(|| Lexer::new(&src).lex()));
}

fn bench_operators(c: &mut Criterion) {
    let src = "+ - * / = /= < > <= >= := <- -> ~> => |> :: .. ..< ... ".repeat(50);
    let _ = c.bench_function("lex_operators", |b| b.iter(|| Lexer::new(&src).lex()));
}

fn bench_mixed(c: &mut Criterion) {
    let src = r#"
let Point := data { ; x : Float ; y : Float };
let origin := .{ x := 0.0, y := 0.0 };
let dist := (p : Point) : Float => (p.x * p.x + p.y * p.y);
let Option [T] := data { | Some : T | None };
let map [A, B] := (opt : Option[A], f : A -> B) : Option[B] =>
    case opt of (
        | .Some(x) => .Some(f(x))
        | .None => .None
    );
"#
    .repeat(50);
    let _ = c.bench_function("lex_mixed", |b| b.iter(|| Lexer::new(&src).lex()));
}

fn bench_strings(c: &mut Criterion) {
    let src = r#""hello" "world" "foo\nbar" "escaped\"quote" "unicode\u{1F600}" "#.repeat(100);
    let _ = c.bench_function("lex_strings", |b| b.iter(|| Lexer::new(&src).lex()));
}

fn bench_comments(c: &mut Criterion) {
    let src = "// line comment\n/* block comment */ x // another\n".repeat(200);
    let _ = c.bench_function("lex_comments", |b| b.iter(|| Lexer::new(&src).lex()));
}

criterion_group!(
    lex_benches,
    bench_identifiers,
    bench_numbers,
    bench_operators,
    bench_mixed,
    bench_strings,
    bench_comments,
);
criterion_main!(lex_benches);
