#![allow(clippy::unwrap_used)]

use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use music_storage::{Arena, Idx};
use std::hint::black_box;

const SIZES: [usize; 3] = [100, 1_000, 10_000];

fn to_u64(n: usize) -> u64 {
    u64::try_from(n).unwrap()
}

fn bench_alloc_sequential(c: &mut Criterion) {
    let mut group = c.benchmark_group("alloc_sequential");
    for &n in &SIZES {
        let _ = group.bench_with_input(BenchmarkId::from_parameter(n), &n, |b, &n| {
            b.iter(|| {
                let mut arena: Arena<u64> = Arena::with_capacity(n);
                for i in 0..n {
                    let _ = black_box(arena.alloc(to_u64(i)));
                }
                arena
            });
        });
    }
    group.finish();
}

fn bench_get_sequential(c: &mut Criterion) {
    let mut group = c.benchmark_group("get_sequential");
    for &n in &SIZES {
        let mut arena: Arena<u64> = Arena::with_capacity(n);
        let indices: Vec<Idx<u64>> = (0..n).map(|i| arena.alloc(to_u64(i))).collect();

        let _ = group.bench_with_input(BenchmarkId::from_parameter(n), &n, |b, &_n| {
            b.iter(|| {
                let mut sum = 0u64;
                for &idx in &indices {
                    sum = sum.wrapping_add(*arena.get(idx));
                }
                let _ = black_box(sum);
            });
        });
    }
    group.finish();
}

fn bench_get_random(c: &mut Criterion) {
    let mut group = c.benchmark_group("get_random");
    for &n in &SIZES {
        let mut arena: Arena<u64> = Arena::with_capacity(n);
        let mut indices: Vec<Idx<u64>> = (0..n).map(|i| arena.alloc(to_u64(i))).collect();

        let mut rng = 0xDEAD_BEEF_u64;
        for i in (1..indices.len()).rev() {
            rng ^= rng << 13;
            rng ^= rng >> 7;
            rng ^= rng << 17;
            let bucket = u64::try_from(i + 1).unwrap();
            let j = usize::try_from(rng % bucket).unwrap();
            indices.swap(i, j);
        }

        let _ = group.bench_with_input(BenchmarkId::from_parameter(n), &n, |b, &_n| {
            b.iter(|| {
                let mut sum = 0u64;
                for &idx in &indices {
                    sum = sum.wrapping_add(*arena.get(idx));
                }
                let _ = black_box(sum);
            });
        });
    }
    group.finish();
}

fn bench_iter(c: &mut Criterion) {
    let mut group = c.benchmark_group("iter");
    for &n in &SIZES {
        let mut arena: Arena<u64> = Arena::with_capacity(n);
        for i in 0..n {
            let _ = arena.alloc(to_u64(i));
        }

        let _ = group.bench_with_input(BenchmarkId::from_parameter(n), &n, |b, &_n| {
            b.iter(|| {
                let mut sum = 0u64;
                for (_idx, val) in &arena {
                    sum = sum.wrapping_add(*val);
                }
                let _ = black_box(sum);
            });
        });
    }
    group.finish();
}

fn bench_alloc_and_get_interleaved(c: &mut Criterion) {
    let mut group = c.benchmark_group("alloc_get_interleaved");
    for &n in &SIZES {
        let _ = group.bench_with_input(BenchmarkId::from_parameter(n), &n, |b, &n| {
            b.iter(|| {
                let mut arena: Arena<u64> = Arena::with_capacity(n);
                let mut prev = arena.alloc(0u64);
                for i in 1..n {
                    let cur = arena.alloc(to_u64(i));
                    let _ = black_box(arena.get(prev));
                    prev = cur;
                }
                arena
            });
        });
    }
    group.finish();
}

criterion_group!(
    arena_benches,
    bench_alloc_sequential,
    bench_get_sequential,
    bench_get_random,
    bench_iter,
    bench_alloc_and_get_interleaved,
);
criterion_main!(arena_benches);
