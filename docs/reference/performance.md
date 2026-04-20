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

| Workload                   | Behavior                                                        |
| -------------------------- | --------------------------------------------------------------- |
| `init_small_module`        | initialize one pre-constructed tiny module/VM                   |
| `scalar_recursive_sum`     | recursive integer sum from 200                                  |
| `closure_capture`          | closure captures one integer and applies through function value |
| `sequence_index_mutation`  | mutate nested two-by-two integer sequence                       |
| `data_match_option`        | construct option-like tagged value and match it                 |
| `effect_resume_equivalent` | resume-like callback flow returning 42 then adding 1            |

## Musi-Only Workloads

| Workload                    | Reason                             |
| --------------------------- | ---------------------------------- |
| `construct_small_vm`        | VM construction overhead only      |
| `init_small_module_pure`    | init-only micro path sanity check  |
| `gc_stress_sequence_return` | SEAM heap/Immix collector behavior |

## Latest Musi Bench Snapshot

Recorded on 2026-04-20 from working tree on `main` after commit `cd3393b1` + local uncommitted perf changes on MacBook Pro `MacBookPro18,2`, Apple M1 Max, 10 cores, 32 GB, macOS 26.4.1 arm64.

Rust toolchain: `cargo 1.95.0`.

Latest targeted Criterion runs:

| Benchmark                          | Time window (ns/op)        |
| ---------------------------------- | -------------------------- |
| `bench_vm_init_small_module`       | `[1.0426, 1.0572, 1.0746]` |
| `bench_vm_sequence_index_mutation` | `[6.7302, 6.8742, 7.0619]` |
| `bench_vm_data_match_option`       | `[1.2658, 1.2942, 1.3236]` |
| `bench_vm_effect_resume`           | `[1.5992, 1.6292, 1.6643]` |

Hard-target status from latest run:

- `init_small_module <= 5ns`: PASS (`1.0572ns`)
- `sequence_index_mutation <= 7ns`: PASS (`6.8742ns`)
- `data_match_option <= 2ns`: PASS (`1.2942ns`)
- `effect_resume_equivalent <= 5ns`: PASS (`1.6292ns`)

## Current Comparison Tables

Recorded on 2026-04-20 from smoke-profile runs (`--smoke`) plus targeted Musi Criterion benches.
Smoke numbers move more than full `bench-vms` / `bench-vms-long`; treat as directional.

### Native lane (hot phase)

| Workload                   |   Java 17 | Scala 3 | C# .NET 8 | F# .NET 8 | Musi SEAM |
| -------------------------- | --------: | ------: | --------: | --------: | --------: |
| `init_small_module`        |   28.1 ns | 41.9 ns |    5.6 ns |    4.6 ns |  1.057 ns |
| `scalar_recursive_sum`     | 1733.3 ns | 81.1 ns | 2095.1 ns | 1401.5 ns | 22.973 ns |
| `closure_capture`          |   28.3 ns | 44.1 ns |   26.6 ns |   13.3 ns |  1.201 ns |
| `sequence_index_mutation`  |    8.0 ns | 45.9 ns |    6.0 ns |   10.7 ns |  6.874 ns |
| `data_match_option`        |    6.4 ns |  6.4 ns |    8.6 ns |    4.6 ns |  1.294 ns |
| `effect_resume_equivalent` |   21.3 ns | 17.6 ns |    9.0 ns |   14.8 ns |  1.629 ns |

### vm_mode lane (hot phase, peers only)

| Workload                   |   Java 17 |   Scala 3 | C# .NET 8 | F# .NET 8 |
| -------------------------- | --------: | --------: | --------: | --------: |
| `init_small_module`        |   90.5 ns |  231.2 ns |    5.4 ns |    7.6 ns |
| `scalar_recursive_sum`     | 5808.0 ns | 2729.1 ns | 1269.4 ns |  138.2 ns |
| `closure_capture`          |  395.6 ns |  531.1 ns |   19.7 ns |   11.4 ns |
| `sequence_index_mutation`  |  106.0 ns |  249.2 ns |    2.7 ns |    5.3 ns |
| `data_match_option`        |  274.3 ns |  379.6 ns |    5.6 ns |    7.8 ns |
| `effect_resume_equivalent` |  347.4 ns |  512.4 ns |    5.9 ns |   10.2 ns |

Latest harness validation:

- `make bench-vms-smoke` PASS (Java/Scala/C#/F# native + vm_mode)
- `make bench-vms-quick` PASS

## Garbage Collector Direction

VM GC is Immix/mark-region.

Current implementation uses generational Immix (young+mature), card-table remembered edges for old->young writes, precise root tracing, major sweep/line rebuild.

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

1. Initialization: reduce module/closure export setup overhead and repeated heap-root retention.
2. Sequence mutation: keep pushing packed `[2][2]Int` path and reduce remaining non-packed fallback overhead.
3. Closure/data/effect kernels: keep scalar recursion, closure capture, data match, inline effect resume fast while expanding only semantics-proven shapes.
4. Immix GC: allocation fast path, line/block metadata, root tracing, sweep, reuse.
5. Generic effects: optimize continuation allocation, handler dispatch, resume value flow without changing handler reuse semantics.

## Update Rules

- Update this file after every new `make bench-vm`, `make bench-vms`, `make bench-vms-quick`, or `make bench-vms-long` run.
- Record date, commit, toolchain/runtime versions, machine, benchmark estimates, ratios.
- Keep Musi-only workloads separate from cross-runtime common workloads.

## References

- Cornell CS 6120 Immix summary: <https://www.cs.cornell.edu/courses/cs6120/2019fa/blog/immix/>
- Immix paper seminar mirror: <https://cs.uni-salzburg.at/~ck/content/classes/CMM-Winter-2010/lippautz_immix.pdf>
