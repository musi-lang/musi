use criterion::{criterion_group, criterion_main, Criterion};
use music_lex::Lexer;
use music_parse::parse;
use music_shared::Interner;

fn bench_expressions(c: &mut Criterion) {
    let src = "(a + b * c) |> f |> g;\n".repeat(200);
    let (tokens, _) = Lexer::new(&src).lex();
    let _ = c.bench_function("parse_expressions", |b| {
        b.iter(|| {
            let mut interner = Interner::new();
            parse(&tokens, &src, &mut interner)
        })
    });
}

fn bench_let_bindings(c: &mut Criterion) {
    let src = "let x := 42;\nlet f := (a : Int) : Int => a + 1;\n".repeat(100);
    let (tokens, _) = Lexer::new(&src).lex();
    let _ = c.bench_function("parse_let_bindings", |b| {
        b.iter(|| {
            let mut interner = Interner::new();
            parse(&tokens, &src, &mut interner)
        })
    });
}

fn bench_data_types(c: &mut Criterion) {
    let src = "let T := data { | A : Int | B | C : Float };\n".repeat(100);
    let (tokens, _) = Lexer::new(&src).lex();
    let _ = c.bench_function("parse_data_types", |b| {
        b.iter(|| {
            let mut interner = Interner::new();
            parse(&tokens, &src, &mut interner)
        })
    });
}

fn bench_case(c: &mut Criterion) {
    let src = "case x of ( | .A(v) => v + 1 | .B => 0 | .C(f) => f );\n".repeat(100);
    let (tokens, _) = Lexer::new(&src).lex();
    let _ = c.bench_function("parse_case", |b| {
        b.iter(|| {
            let mut interner = Interner::new();
            parse(&tokens, &src, &mut interner)
        })
    });
}

fn bench_mixed_realistic(c: &mut Criterion) {
    let src = r#"
import "std/option" as Option;
import "std/list" as List;

let Point := data { ; x : Float ; y : Float };
let Shape := data { | Circle : Float | Rect : Point | Empty };

let Functor := class {
    let map[A, B](self : F[A], f : A -> B) : F[B]
};

instance Functor[Option] {
    let map[A, B](self : Option[A], f : A -> B) : Option[B] :=
        case self of (
            | .Some(x) => .Some(f(x))
            | .None => .None
        )
};

let origin := .{ x := 0.0, y := 0.0 };
let dist := (p : Point) : Float => (p.x * p.x + p.y * p.y);
let area := (s : Shape) : Float =>
    case s of (
        | .Circle(r) => 3.14159 * r * r
        | .Rect(p) => p.x * p.y
        | .Empty => 0.0
    );
let squares := [x * x | x in xs];
let pipeline := input |> parse |> validate |> transform;
let greet := (name : String) : String => f"Hello, {name}!";

export let main := () => (
    let p := .{ x := 3.0, y := 4.0 };
    let d := dist(p);
    let s := .Circle(5.0);
    let a := area(s);
    let mapped := .Some(42) |> map((x) => x + 1);
    mapped
);
"#
    .repeat(10);
    let (tokens, _) = Lexer::new(&src).lex();
    let _ = c.bench_function("parse_mixed_realistic", |b| {
        b.iter(|| {
            let mut interner = Interner::new();
            parse(&tokens, &src, &mut interner)
        })
    });
}

fn bench_deeply_nested(c: &mut Criterion) {
    let src = "((((((a + b) * c) - d) / e) % f) + g);\n".repeat(100);
    let (tokens, _) = Lexer::new(&src).lex();
    let _ = c.bench_function("parse_deeply_nested", |b| {
        b.iter(|| {
            let mut interner = Interner::new();
            parse(&tokens, &src, &mut interner)
        })
    });
}

criterion_group!(
    parse_benches,
    bench_expressions,
    bench_let_bindings,
    bench_data_types,
    bench_case,
    bench_mixed_realistic,
    bench_deeply_nested,
);
criterion_main!(parse_benches);
