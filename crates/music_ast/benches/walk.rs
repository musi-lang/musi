use criterion::{Criterion, criterion_group, criterion_main};
use music_ast::ExprId;
use music_ast::data::AstData;
use music_ast::expr::{BinOp, ExprKind};
use music_ast::walk::map_expr_children;
use music_shared::{Literal, Span, Spanned};

fn alloc(ast: &mut AstData, kind: ExprKind) -> ExprId {
    ast.exprs.alloc(Spanned::new(kind, Span::DUMMY))
}

fn build_flat_seq(n: usize) -> (AstData, ExprId) {
    let mut ast = AstData::new();
    let items: Vec<_> = (0..n)
        .map(|i| {
            let val = i64::try_from(i).expect("bench index fits i64");
            alloc(&mut ast, ExprKind::Lit(Literal::Int(val)))
        })
        .collect();
    let seq = alloc(&mut ast, ExprKind::Seq(items));
    (ast, seq)
}

fn build_deep_binop(depth: usize) -> (AstData, ExprId) {
    let mut ast = AstData::new();
    let mut current = alloc(&mut ast, ExprKind::Lit(Literal::Int(0)));
    for i in 1..=depth {
        let val = i64::try_from(i).expect("bench index fits i64");
        let rhs = alloc(&mut ast, ExprKind::Lit(Literal::Int(val)));
        current = alloc(&mut ast, ExprKind::BinOp(BinOp::Add, current, rhs));
    }
    (ast, current)
}

fn bench_walk_flat(c: &mut Criterion) {
    let (mut ast, seq) = build_flat_seq(1000);
    let _ = c.bench_function("walk_flat_1000", |b| {
        b.iter(|| map_expr_children(&mut ast, seq, &mut |_, id| id))
    });
}

fn bench_walk_deep(c: &mut Criterion) {
    let (mut ast, root) = build_deep_binop(100);
    let _ = c.bench_function("walk_deep_100", |b| {
        b.iter(|| map_expr_children(&mut ast, root, &mut |_, id| id))
    });
}

fn bench_walk_identity(c: &mut Criterion) {
    let (mut ast, root) = build_deep_binop(500);
    let _ = c.bench_function("walk_identity_500", |b| {
        b.iter(|| map_expr_children(&mut ast, root, &mut |_, id| id))
    });
}

criterion_group!(
    walk_benches,
    bench_walk_flat,
    bench_walk_deep,
    bench_walk_identity,
);
criterion_main!(walk_benches);
