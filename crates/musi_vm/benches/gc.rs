#![allow(clippy::unwrap_used)]

use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use musi_vm::heap::Heap;
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
                    heap.sweep();
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
                    heap.sweep();
                },
                criterion::BatchSize::SmallInput,
            );
        });
    }
    let _ = group.finish();
}

fn bench_sweep(c: &mut Criterion) {
    let mut group = c.benchmark_group("sweep");
    for (mark_pct, label) in [(10, "10pct"), (50, "50pct"), (90, "90pct")] {
        let _ = group.bench_with_input(
            BenchmarkId::new("10k_objects", label),
            &mark_pct,
            |b, &pct| {
                b.iter_batched(
                    || {
                        let mut heap = Heap::new();
                        let mut indices = Vec::with_capacity(10_000);
                        for _ in 0..10_000 {
                            let idx = heap.alloc_array(Value::UNIT, vec![Value::from_int(1)]);
                            indices.push(idx);
                        }
                        let mark_count = 10_000 * pct / 100;
                        for &idx in &indices[..mark_count] {
                            heap.mark_object(idx);
                        }
                        heap
                    },
                    |mut heap| {
                        heap.sweep();
                    },
                    criterion::BatchSize::SmallInput,
                );
            },
        );
    }
    let _ = group.finish();
}

fn bench_full_gc_cycle(c: &mut Criterion) {
    let mut group = c.benchmark_group("full_gc");
    for size in [100, 1000, 5000] {
        let _ = group.bench_with_input(BenchmarkId::new("cycle", size), &size, |b, &n| {
            b.iter_batched(
                || {
                    let mut heap = Heap::new();
                    let mut live = Vec::new();
                    for i in 0..n {
                        let idx = heap.alloc_array(Value::UNIT, vec![Value::from_int(i as i64)]);
                        if i % 3 == 0 {
                            live.push(idx);
                        }
                    }
                    (heap, live)
                },
                |(mut heap, live)| {
                    for &idx in &live {
                        heap.mark_object(idx);
                    }
                    heap.sweep();
                    heap.reset_threshold();
                },
                criterion::BatchSize::SmallInput,
            );
        });
    }
    let _ = group.finish();
}

fn bench_free_list_reuse(c: &mut Criterion) {
    let _ = c.bench_function("free_list_reuse_1000", |b| {
        b.iter_batched(
            || {
                let mut heap = Heap::new();
                for _ in 0..1000 {
                    let _ = heap.alloc_string("temp".into());
                }
                heap.sweep();
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

criterion_group!(
    gc_benches,
    bench_alloc_throughput,
    bench_mark_object_graph,
    bench_sweep,
    bench_full_gc_cycle,
    bench_free_list_reuse,
);
criterion_main!(gc_benches);
