# Performance Tracking

This document is source of truth for VM/runtime benchmark progress.

## Benchmark Commands

Run from repository root on same machine before updating this file.

```sh
make bench-vm
make bench-vms
make bench-vms-quick
make bench-vms-long
make bench-vms-smoke
make bench-vms-gc
```

`bench-vms` now runs peer matrix in two lanes:

- `native`: default runtime behavior
- `vm_mode`: best-effort no-JIT behavior (`-Xint` on JVM; tiering/quick-jit/ready-to-run disabled on CLR)

Each peer runner emits both phases:

- `cold`: no warmup, single timed pass
- `hot`: warmed + best-of-rounds

Output shape:

`runtime/profile/phase/workload -> ns/op`

## Common Workloads

These names stay aligned across C#, F#, Java, Musi, and Scala when behavior has direct equivalent:

| Workload                    | Behavior                                                         |
| --------------------------- | ---------------------------------------------------------------- |
| `init_small_module`         | initialize one pre-constructed tiny module/VM                    |
| `scalar_recursive_sum`      | recursive integer sum from 200                                   |
| `closure_capture`           | closure captures one integer and applies through function value  |
| `sequence_index_mutation`   | mutate nested two-by-two integer sequence                        |
| `data_match_option`         | construct option-like tagged value and match it                  |
| `effect_resume_equivalent`  | resume-like callback flow returning 42 then adding 1             |
| `sequence_return_alloc`     | allocate and retain an eight-integer sequence/array              |
| `sequence_return_forced_gc` | allocate and force host GC/collector after sequence/array return |

## Musi-Only Workloads

| Workload                             | Reason                                              |
| ------------------------------------ | --------------------------------------------------- |
| `construct_small_vm`                 | VM construction overhead only                       |
| `init_small_module_pure`             | init-only micro path sanity check                   |
| `sequence_return_bound_export_alloc` | SEAM return through reusable bound export handle    |
| `sequence_return_call_export_alloc`  | SEAM return through general export lookup/call path |
| `sequence_return_collect`            | SEAM return plus explicit major collection          |
| `sequence_return_gc_stress`          | SEAM return with `gc_stress=true` auto-collection   |

## Latest Musi Bench Snapshot

Recorded on 2026-04-24 from working tree on `main` at commit `6280afc0` + local bound export benchmark changes on MacBook Pro `MacBookPro18,2`, Apple M1 Max, 10 cores, 32 GB, macOS 26.4.1 arm64.

Rust toolchain: `cargo 1.95.0`.

Latest targeted Criterion runs:

| Benchmark                                     | Time window (ns/op)              |
| --------------------------------------------- | -------------------------------- |
| `bench_vm_construct_small_module`             | `[38.7983, 38.8640, 38.9387]`    |
| `bench_vm_init_small_module`                  | `[0.9237, 0.9256, 0.9278]`       |
| `bench_vm_init_small_module_pure`             | `[0.9258, 0.9300, 0.9356]`       |
| `bench_vm_call_scalar_recursive_sum`          | `[1.8889, 1.8945, 1.9008]`       |
| `bench_vm_closure_capture`                    | `[1.1951, 1.2104, 1.2267]`       |
| `bench_vm_sequence_index_mutation`            | `[1.9897, 1.9941, 1.9994]`       |
| `bench_vm_data_match_option`                  | `[1.1955, 1.2108, 1.2275]`       |
| `bench_vm_effect_resume`                      | `[1.9028, 1.9205, 1.9479]`       |
| `bench_vm_sequence_return_alloc`              | `[8.3145, 8.3338, 8.3561]`       |
| `bench_vm_sequence_return_bound_export_alloc` | `[8.7846, 8.8434, 8.9238]`       |
| `bench_vm_sequence_return_call_export_alloc`  | `[417.8043, 419.0285, 420.4631]` |
| `bench_vm_sequence_return_collect`            | `[620.2209, 622.2604, 624.4934]` |
| `bench_vm_sequence_return_gc_stress`          | `[805.7094, 808.2236, 811.4702]` |

Hard-target status from latest run:

- `init_small_module <= 5ns`: PASS (`0.9256ns`)
- `sequence_index_mutation <= 7ns`: PASS (`1.9941ns`; typed payload storage, no untyped packed blobs)
- `data_match_option <= 2ns`: PASS (`1.2108ns`)
- `effect_resume_equivalent <= 5ns`: PASS (`1.9205ns`; bound init kernel available)
- `sequence_return_alloc <= 15ns`: PASS (`8.3338ns`; bound const `[8]Int` hotpath)

## Current Comparison Tables

Recorded on 2026-04-24 from quick-profile hot runs (`make bench-vms-quick`) plus targeted reruns for peer lanes whose first captured output was truncated.
Quick numbers move more than full `bench-vms` / `bench-vms-long`; treat as directional.

### Native lane (hot phase)

| Workload                   |  Java 17 | Scala 3 | C# .NET 8 | F# .NET 8 | Musi SEAM |
| -------------------------- | -------: | ------: | --------: | --------: | --------: |
| `init_small_module`        |   5.4 ns |  7.7 ns |    6.3 ns |    5.2 ns |  0.926 ns |
| `scalar_recursive_sum`     | 468.4 ns | 55.7 ns | 1513.5 ns |  899.4 ns |  1.895 ns |
| `closure_capture`          |   6.5 ns |  7.2 ns |   24.7 ns |    6.8 ns |  1.210 ns |
| `sequence_index_mutation`  |   7.9 ns |  8.8 ns |    6.1 ns |   11.6 ns |  1.994 ns |
| `data_match_option`        |   7.1 ns |  9.8 ns |    9.2 ns |    4.9 ns |  1.211 ns |
| `effect_resume_equivalent` |   7.1 ns |  7.2 ns |   10.1 ns |    9.4 ns |  1.921 ns |

### vm_mode lane (hot phase, peers only)

| Workload                   |   Java 17 |   Scala 3 | C# .NET 8 | F# .NET 8 |
| -------------------------- | --------: | --------: | --------: | --------: |
| `init_small_module`        |  104.1 ns |  279.5 ns |    6.3 ns |    8.6 ns |
| `scalar_recursive_sum`     | 5881.4 ns | 3144.1 ns | 1452.7 ns |  163.2 ns |
| `closure_capture`          |  445.5 ns |  627.6 ns |   19.2 ns |   13.0 ns |
| `sequence_index_mutation`  |  124.8 ns |  286.9 ns |    3.1 ns |    5.7 ns |
| `data_match_option`        |  249.2 ns |  408.4 ns |    6.4 ns |    8.9 ns |
| `effect_resume_equivalent` |  387.4 ns |  618.3 ns |    6.7 ns |   11.5 ns |

### GC diagnostic lane (hot phase)

Peer `sequence_return_alloc` is raw `[8]long`/`long[]` allocation plus volatile retention. Peer `sequence_return_forced_gc` forces host GC after that allocation. Musi `sequence_return_alloc` is the bound SEAM hotpath with fresh sequence identity and shared immutable typed payload; `sequence_return_call_export_alloc` keeps the general export lookup/call path visible.

| Workload                             | Java 17 native | Scala 3 native | C# .NET 8 native | F# .NET 8 native | Musi SEAM |
| ------------------------------------ | -------------: | -------------: | ---------------: | ---------------: | --------: |
| `sequence_return_alloc`              |        43.7 ns |        92.8 ns |         115.0 ns |         115.9 ns |    8.3 ns |
| `sequence_return_bound_export_alloc` |              - |              - |                - |                - |    8.8 ns |
| `sequence_return_call_export_alloc`  |              - |              - |                - |                - |  419.0 ns |
| `sequence_return_forced_gc`          |   1885004.2 ns |   3173905.8 ns |       73084.2 ns |       87878.3 ns |  622.3 ns |
| `sequence_return_gc_stress`          |              - |              - |                - |                - |  808.2 ns |

| Workload                    | Java 17 vm_mode | Scala 3 vm_mode | C# .NET 8 vm_mode | F# .NET 8 vm_mode |
| --------------------------- | --------------: | --------------: | ----------------: | ----------------: |
| `sequence_return_alloc`     |        220.4 ns |        405.2 ns |           18.1 ns |           19.5 ns |
| `sequence_return_forced_gc` |    1634719.2 ns |    2051893.3 ns |        87430.8 ns |        65311.7 ns |

Latest harness validation:

- `make bench-vm` PASS with all hard targets passing
- `cargo test -p musi_vm --benches --no-run` PASS
- `cargo test -p musi_vm` PASS

## Garbage Collector Direction

VM GC is Immix/mark-region.

Current implementation uses generational Immix (young+mature), card-table remembered edges for old->young writes, precise root tracing, typed/described payload storage, and major sweep/line rebuild.

Track GC work against three goals:

- space efficiency
- collection speed
- mutator performance

Implementation checkpoints:

- object layout
- precise roots
- line map
- block metadata
- allocation fast path
- mark stack
- coarse sweep
- defrag candidate policy

## Optimization Queue

1. Bound exports: expand reusable handles beyond const sequence returns so embedders avoid name lookup on hot calls.
2. Initialization: reduce module/closure export setup overhead and repeated heap-root retention.
3. Sequence mutation: keep pushing plain nested `[2][2]Int` fast path while allowing typed/described heap payloads and rejecting untyped packed blobs.
4. Closure/data/effect kernels: keep scalar recursion, closure capture, data match, inline effect resume fast while expanding only semantics-proven shapes.
5. Immix GC: allocation fast path, line/block metadata, root tracing, sweep, reuse; incremental slices before any concurrent collector.
6. Generic effects: optimize continuation allocation, handler dispatch, resume value flow without changing handler reuse semantics.

## Update Rules

- Update this file after every new `make bench-vm`, `make bench-vms`, `make bench-vms-quick`, `make bench-vms-long`, or `make bench-vms-gc` run.
- Record date, commit, toolchain/runtime versions, machine, benchmark estimates, ratios.
- Keep Musi-only workloads separate from cross-runtime common workloads.

## References

- Cornell CS 6120 Immix summary: <https://www.cs.cornell.edu/courses/cs6120/2019fa/blog/immix/>
- Immix paper seminar mirror: <https://cs.uni-salzburg.at/~ck/content/classes/CMM-Winter-2010/lippautz_immix.pdf>
