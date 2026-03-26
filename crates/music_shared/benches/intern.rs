use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use music_shared::Interner;

fn bench_intern_unique(c: &mut Criterion) {
    let mut group = c.benchmark_group("intern_unique");
    for size in [100, 1000, 10_000] {
        let strings: Vec<String> = (0..size).map(|i| format!("var_{i}")).collect();
        let _ =
            group.bench_with_input(BenchmarkId::from_parameter(size), &strings, |b, strings| {
                b.iter(|| {
                    let mut interner = Interner::new();
                    for s in strings {
                        let _ = interner.intern(s);
                    }
                });
            });
    }
    let _ = group.finish();
}

fn bench_intern_repeated(c: &mut Criterion) {
    let mut group = c.benchmark_group("intern_repeated");
    let fixed: Vec<String> = (0..50).map(|i| format!("repeated_{i}")).collect();
    for size in [100, 1000, 10_000] {
        let _ = group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter(|| {
                let mut interner = Interner::new();
                for i in 0..size {
                    let _ = interner.intern(&fixed[i % fixed.len()]);
                }
            });
        });
    }
    let _ = group.finish();
}

fn bench_resolve(c: &mut Criterion) {
    let mut group = c.benchmark_group("resolve");
    for size in [100, 1000, 10_000] {
        let mut interner = Interner::new();
        let symbols: Vec<_> = (0..size)
            .map(|i| interner.intern(&format!("sym_{i}")))
            .collect();
        let _ = group.bench_with_input(
            BenchmarkId::from_parameter(size),
            &(interner, symbols),
            |b, (interner, symbols)| {
                b.iter(|| {
                    for &sym in symbols {
                        let _ = interner.resolve(sym);
                    }
                });
            },
        );
    }
    let _ = group.finish();
}

fn bench_intern_identifiers(c: &mut Criterion) {
    let identifiers = [
        "let", "data", "case", "of", "if", "x", "y", "z", "f", "g", "Int", "Float", "Bool", "Unit",
        "True", "False", "Eq", "Ord", "Num", "Show", "result", "value", "acc", "idx", "len",
    ];

    let mut group = c.benchmark_group("intern_identifiers");
    for size in [100, 1000, 10_000] {
        let _ = group.bench_with_input(BenchmarkId::from_parameter(size), &size, |b, &size| {
            b.iter(|| {
                let mut interner = Interner::new();
                for i in 0..size {
                    let _ = interner.intern(identifiers[i % identifiers.len()]);
                }
            });
        });
    }
    let _ = group.finish();
}

fn bench_intern_mixed(c: &mut Criterion) {
    let mut group = c.benchmark_group("intern_mixed");
    for size in [100, 1000, 10_000] {
        let unique_strings: Vec<String> = (0..100).map(|i| format!("unique_{i}")).collect();
        let _ = group.bench_with_input(
            BenchmarkId::from_parameter(size),
            &(size, unique_strings),
            |b, (size, unique_strings)| {
                b.iter(|| {
                    let mut interner = Interner::new();
                    for i in 0..*size {
                        if i < 100 {
                            let _ = interner.intern(&unique_strings[i]);
                        } else {
                            let _ = interner.intern(&unique_strings[i % 100]);
                        }
                    }
                });
            },
        );
    }
    let _ = group.finish();
}

criterion_group!(
    benches,
    bench_intern_unique,
    bench_intern_repeated,
    bench_resolve,
    bench_intern_identifiers,
    bench_intern_mixed,
);
criterion_main!(benches);
