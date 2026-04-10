use std::fmt::Write as _;
use std::hint::black_box;

use criterion::{Criterion, criterion_group, criterion_main};

use music_base::SourceMap;

fn bench_line_col(c: &mut Criterion) {
    let mut sources = SourceMap::new();
    let mut text = String::new();
    for i in 0..2_000 {
        if writeln!(&mut text, "line {i:04} = 123 + 456").is_err() {
            break;
        }
    }
    let id = sources.add("bench.ms", text).expect("add succeeds");
    let source = sources.get(id).expect("source exists");

    let _ = c.bench_function("bench_source_line_col", |b| {
        b.iter(|| {
            let (line, col) = source.line_col(black_box(12_345));
            let _ = black_box((line, col));
        });
    });

    let _ = c.bench_function("bench_source_line_text", |b| {
        b.iter(|| {
            let line = source.line_text(black_box(1_000));
            let _ = black_box(line);
        });
    });
}

criterion_group!(benches, bench_line_col);
criterion_main!(benches);
