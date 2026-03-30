use criterion::{Criterion, criterion_group, criterion_main};
use music_names::Interner;

fn bench_intern_unique(c: &mut Criterion) {
    let inputs = (0..10_000)
        .map(|idx| format!("name_{idx}"))
        .collect::<Vec<_>>();
    let _ = c.bench_function("interner_unique", |b| {
        b.iter(|| {
            let mut interner = Interner::new();
            for input in &inputs {
                let _ = interner.intern(input);
            }
        });
    });
}

fn bench_intern_repeated(c: &mut Criterion) {
    let input = "musi_symbol";
    let _ = c.bench_function("interner_repeated", |b| {
        b.iter(|| {
            let mut interner = Interner::new();
            for _ in 0..10_000 {
                let _ = interner.intern(input);
            }
        });
    });
}

criterion_group!(interner_benches, bench_intern_unique, bench_intern_repeated);
criterion_main!(interner_benches);
