#![allow(clippy::unwrap_used)]

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use musi_vm::internal::Heap;
use musi_vm::value::Value;

fn bench_alloc_throughput(c: &mut Criterion) {
    let mut group = c.benchmark_group("alloc");
    for size in [10, 100, 1000, 10_000] {
        let _ = group.bench_with_input(BenchmarkId::new("arrays", size), &size, |b, &n| {
            b.iter(|| {
                let mut heap = Heap::new();
                for _ in 0..n {
                    let _ = heap.alloc_array(Value::UNIT, vec![Value::UNIT; 4]);
                }
            });
        });
        let _ = group.bench_with_input(BenchmarkId::new("strings", size), &size, |b, &n| {
            b.iter(|| {
                let mut heap = Heap::new();
                for i in 0..n {
                    let _ = heap.alloc_string(format!("string_{i}"));
                }
            });
        });
    }
    let _ = group.finish();
}

fn bench_mark_object_graph(c: &mut Criterion) {
    let mut group = c.benchmark_group("mark");
    for size in [100, 1000, 10_000] {
        let _ = group.bench_with_input(BenchmarkId::new("chain", size), &size, |b, &n| {
            b.iter_batched(
                || {
                    let mut heap = Heap::new();
                    let mut prev = heap.alloc_array(Value::UNIT, vec![]);
                    for _ in 1..n {
                        let ptr = Value::from_ptr(prev);
                        prev = heap.alloc_array(Value::UNIT, vec![ptr]);
                    }
                    (heap, prev)
                },
                |(mut heap, root)| {
                    heap.mark_object(root);
                },
                criterion::BatchSize::SmallInput,
            );
        });
        let _ = group.bench_with_input(BenchmarkId::new("wide", size), &size, |b, &n| {
            b.iter_batched(
                || {
                    let mut heap = Heap::new();
                    let mut ptrs = Vec::with_capacity(n);
                    for _ in 0..n {
                        let idx = heap.alloc_array(Value::UNIT, vec![Value::from_int(42)]);
                        ptrs.push(Value::from_ptr(idx));
                    }
                    let root = heap.alloc_array(Value::UNIT, ptrs);
                    (heap, root)
                },
                |(mut heap, root)| {
                    heap.mark_object(root);
                },
                criterion::BatchSize::SmallInput,
            );
        });
    }
    let _ = group.finish();
}

fn bench_immix_minor_collect(c: &mut Criterion) {
    let mut group = c.benchmark_group("immix_minor");
    for (survival_pct, label) in [(10, "10pct"), (50, "50pct"), (90, "90pct")] {
        let _ = group.bench_with_input(
            BenchmarkId::new("510_cells", label),
            &survival_pct,
            |b, &pct| {
                b.iter_batched(
                    || {
                        let mut heap = Heap::new();
                        let mut all = Vec::with_capacity(510);
                        for _ in 0..510 {
                            let idx = heap.alloc_array(Value::UNIT, vec![Value::from_int(1)]);
                            all.push(Value::from_ptr(idx));
                        }
                        let survive_count = 510 * pct / 100;
                        let roots: Vec<Value> = all[..survive_count].to_vec();
                        (heap, roots)
                    },
                    |(mut heap, roots)| {
                        heap.collect_minor(&roots);
                    },
                    criterion::BatchSize::SmallInput,
                );
            },
        );
    }
    let _ = group.finish();
}

fn bench_immix_line_sweep(c: &mut Criterion) {
    let mut group = c.benchmark_group("immix_sweep");
    for block_count in [1, 10, 100] {
        let _ = group.bench_with_input(
            BenchmarkId::new("blocks", block_count),
            &block_count,
            |b, &n| {
                b.iter_batched(
                    || {
                        let mut heap = Heap::new();
                        let cells_needed = n * 510;
                        let mut roots = Vec::new();
                        for i in 0..cells_needed {
                            let idx = heap.alloc_array(Value::UNIT, vec![Value::from_int(1)]);
                            if i % 3 == 0 {
                                roots.push(Value::from_ptr(idx));
                            }
                        }
                        // Mark some as live
                        for &r in &roots {
                            heap.mark_value(r);
                        }
                        heap
                    },
                    |mut heap| {
                        heap.collect_major(&[]);
                    },
                    criterion::BatchSize::SmallInput,
                );
            },
        );
    }
    let _ = group.finish();
}

fn bench_immix_hole_reuse(c: &mut Criterion) {
    let _ = c.bench_function("immix_hole_reuse_1000", |b| {
        b.iter_batched(
            || {
                let mut heap = Heap::new();
                let mut keep = Vec::new();
                for i in 0..1000 {
                    let idx = heap.alloc_string("temp".into());
                    if i % 4 == 0 {
                        keep.push(Value::from_ptr(idx));
                    }
                }
                heap.collect_major(&keep);
                heap
            },
            |mut heap| {
                for _ in 0..1000 {
                    let _ = heap.alloc_string("reused".into());
                }
            },
            criterion::BatchSize::SmallInput,
        );
    });
}

fn bench_full_gc_cycle(c: &mut Criterion) {
    let mut group = c.benchmark_group("full_gc");
    for size in [100, 1000, 5000] {
        let _ = group.bench_with_input(BenchmarkId::new("cycle", size), &size, |b, &n| {
            b.iter_batched(
                || {
                    let mut heap = Heap::new();
                    let mut live_vals = Vec::new();
                    for i in 0..n {
                        let idx = heap.alloc_array(
                            Value::UNIT,
                            vec![Value::from_int(i64::try_from(i).unwrap_or(0))],
                        );
                        if i % 3 == 0 {
                            live_vals.push(Value::from_ptr(idx));
                        }
                    }
                    (heap, live_vals)
                },
                |(mut heap, live)| {
                    heap.collect_major(&live);
                    heap.reset_threshold();
                },
                criterion::BatchSize::SmallInput,
            );
        });
    }
    let _ = group.finish();
}

criterion_group!(
    gc_benches,
    bench_alloc_throughput,
    bench_mark_object_graph,
    bench_immix_minor_collect,
    bench_immix_line_sweep,
    bench_immix_hole_reuse,
    bench_full_gc_cycle,
);
criterion_main!(gc_benches);
