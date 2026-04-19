use std::hint;
use std::time::Duration;

use criterion::{Criterion, criterion_group, criterion_main};

use music_syntax::Lexer;

fn repeat_to_approx_bytes(chunk: &str, target_bytes: usize) -> String {
    let mut out = String::with_capacity(target_bytes.saturating_add(chunk.len()));
    while out.len() < target_bytes {
        out.push_str(chunk);
    }
    out
}

fn run_lexer_once(text: &str) -> usize {
    let lexed = Lexer::new(text).lex();
    debug_assert!(lexed.errors().is_empty());
    lexed.tokens().len()
}

fn bench_lex_small_mixed(c: &mut Criterion) {
    let text = hint::black_box(
        r"
--- doc
let add = (+);
let x = 1_000 + 2 * 3;
let y = `hello ${x}`;
",
    );
    _ = c.bench_function("bench_lex_small_mixed", |b| b.iter(|| run_lexer_once(text)));
}

fn bench_lex_large_mixed_1mb(c: &mut Criterion) {
    let chunk = r"
--- doc
let add = (+);
let mut counter = 0;
counter := counter + 1;
let x = 1_000 + 2 * 3;
let y = `hello ${x}`;
let z = `raw template`;
-- comment
/- block comment -/
";
    let source = repeat_to_approx_bytes(chunk, 1_000_000);
    let text = hint::black_box(source.as_str());
    _ = c.bench_function("bench_lex_large_mixed_1mb", |b| {
        b.iter(|| run_lexer_once(text));
    });
}

fn bench_lex_long_string_1mb(c: &mut Criterion) {
    let inner = repeat_to_approx_bytes("a", 1_000_000);
    let source = format!("let s = \"{inner}\";\n");
    let text = hint::black_box(source.as_str());
    _ = c.bench_function("bench_lex_long_string_1mb", |b| {
        b.iter(|| run_lexer_once(text));
    });
}

fn bench_lex_trivia_heavy(c: &mut Criterion) {
    let chunk = " \t  \n-- line\n--- doc\n/- block -/\n/-- doc -/\n\n";
    let source = repeat_to_approx_bytes(chunk, 1_000_000);
    let text = hint::black_box(source.as_str());
    _ = c.bench_function("bench_lex_trivia_heavy", |b| {
        b.iter(|| run_lexer_once(text));
    });
}

fn bench_lex_numeric_heavy(c: &mut Criterion) {
    let chunk = "let a := 1_234_567; let b := 0xff_ff; let c := 0o7_7_7; let d := 0b1_0_1_0; let e := 3.1415; let f := 2e10; let g := .5e-2;\n";
    let source = repeat_to_approx_bytes(chunk, 1_000_000);
    let text = hint::black_box(source.as_str());
    _ = c.bench_function("bench_lex_numeric_heavy", |b| {
        b.iter(|| run_lexer_once(text));
    });
}

fn bench_lex_string_heavy(c: &mut Criterion) {
    let chunk = r#"let s := "hello \"world\" \\n"; let t := `hi ${x}`; let r := 'a'; let e := '\n';
"#;
    let source = repeat_to_approx_bytes(chunk, 1_000_000);
    let text = hint::black_box(source.as_str());
    _ = c.bench_function("bench_lex_string_heavy", |b| {
        b.iter(|| run_lexer_once(text));
    });
}

fn bench_lex_operator_heavy(c: &mut Criterion) {
    let chunk = "let x := 0; x := x + 1; a:?>b a:?T a:?>T a -> b a := b a => b a ~> b a /= b a <= b a >= b a <: b a...b { ...a, x} a.[x] a |> b a ++ b (+) (-) (*);\n";
    let source = repeat_to_approx_bytes(chunk, 1_000_000);
    let text = hint::black_box(source.as_str());
    _ = c.bench_function("bench_lex_operator_heavy", |b| {
        b.iter(|| run_lexer_once(text));
    });
}

fn bench_lex_ident_heavy(c: &mut Criterion) {
    let chunk = "and as match class data effect export foreign handle if import in instance law let mut request not of opaque or quote resume shl shr using where xor alpha beta gamma delta epsilon escaped_name plain_ident_123 another_one_456;\n";
    let source = repeat_to_approx_bytes(chunk, 1_000_000);
    let text = hint::black_box(source.as_str());
    _ = c.bench_function("bench_lex_ident_heavy", |b| b.iter(|| run_lexer_once(text)));
}

fn bench_lex_symbolic_op_long(c: &mut Criterion) {
    let chunk = "let op = (+++++); a +++++ b; a <<<<>>>> b; a ==<<==>>== b;\n";
    let source = repeat_to_approx_bytes(chunk, 1_000_000);
    let text = hint::black_box(source.as_str());
    _ = c.bench_function("bench_lex_symbolic_op_long", |b| {
        b.iter(|| run_lexer_once(text));
    });
}

fn bench_lex_unicode_string_heavy(c: &mut Criterion) {
    let chunk =
        "let s = \"Καλημέρα 🙂🙂🙂\"; let r = 'λ'; let e = '\\u0041'; let f = '\\u000041';\n";
    let source = repeat_to_approx_bytes(chunk, 1_000_000);
    let text = hint::black_box(source.as_str());
    _ = c.bench_function("bench_lex_unicode_string_heavy", |b| {
        b.iter(|| run_lexer_once(text));
    });
}

criterion_group! {
    name = benches_small;
    config = Criterion::default();
    targets =
        bench_lex_small_mixed,
        bench_lex_trivia_heavy,
        bench_lex_numeric_heavy,
        bench_lex_string_heavy,
        bench_lex_ident_heavy,
        bench_lex_symbolic_op_long,
        bench_lex_unicode_string_heavy
}

criterion_group! {
    name = benches_operator;
    config = Criterion::default().measurement_time(Duration::from_secs(8));
    targets = bench_lex_operator_heavy
}

criterion_group! {
    name = benches_large;
    config = Criterion::default().sample_size(10);
    targets = bench_lex_large_mixed_1mb, bench_lex_long_string_1mb
}
criterion_main!(benches_small, benches_operator, benches_large);
