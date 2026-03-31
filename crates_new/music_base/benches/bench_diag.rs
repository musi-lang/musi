use std::hint::black_box;

use criterion::{Criterion, criterion_group, criterion_main};

use music_base::{Diag, SourceMap, Span};
use music_base::diag::emit;

fn bench_emit(c: &mut Criterion) {
    let mut sources = SourceMap::new();
    let text = "let x := 1 + y\nlet z := 2\n".repeat(256);
    let source_id = sources.add("bench.ms", text).expect("add succeeds");

    let diag = Diag::error("expected ';' after expression")
        .with_label(Span::new(10, 11), source_id, "")
        .with_note("add semicolon");

    let _ = c.bench_function("bench_diag_emit_colorless", |b| {
        b.iter(|| {
            let mut buf = Vec::new();
            emit(&mut buf, black_box(&diag), black_box(&sources), false).expect("emit succeeds");
            let _ = black_box(buf);
        });
    });
}

criterion_group!(benches, bench_emit);
criterion_main!(benches);
