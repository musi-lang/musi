# Performance Tracking

This document is the source of truth for VM/runtime benchmark progress.

## Benchmark Commands

Run benchmarks from the repository root on the same machine before updating this file.

```sh
make bench-vm
make bench-vms
make bench-vms-smoke
```

`bench-vms` runs managed-runtime baselines in peer order: Java 17, Scala 3, C# .NET 8, F# .NET 8, Musi SEAM. Musi uses Criterion and SEAM bytecode execution. The other runners use their platform VM/JIT directly. There is no C# interpreter baseline.

## Common Workloads

These names must stay aligned across C#, F#, Java, Musi, and Scala where the behavior has a direct equivalent:

| Workload                   | Behavior                                                        |
| -------------------------- | --------------------------------------------------------------- |
| `init_small_module`        | construct tiny module-like object and read answer               |
| `scalar_recursive_sum`     | recursive integer sum from 200                                  |
| `closure_capture`          | closure captures one integer and applies through function value |
| `sequence_index_mutation`  | mutate nested two-by-two integer sequence                       |
| `data_match_option`        | construct option-like tagged value and match it                 |
| `effect_resume_equivalent` | resume-like callback flow returning 42 then adding 1            |

## Musi-Only Workloads

These stay outside cross-runtime comparison unless an honest behavioral equivalent exists:

| Workload                    | Reason                             |
| --------------------------- | ---------------------------------- |
| `gc_stress_sequence_return` | SEAM heap/Immix collector behavior |

## Current Baseline

Recorded on 2026-04-20 from `327ca532` on MacBook Pro `MacBookPro18,2`, Apple M1 Max, 10 cores, 32 GB, macOS 26.4.1 arm64. Rust used `cargo 1.95.0`; Java used OpenJDK `17.0.18`; Scala used Scala CLI `1.13.0`; .NET SDK was `10.0.202`; CLR runtime reported by the benches was `8.0.26`.

Do not mix historical C# interpreter data with this multi-VM table. Values are point estimates from a full local run. Musi SEAM values are Criterion mean point estimates from `estimates.json`. Lower is better. Musi currently uses SEAM bytecode plus tiered runtime kernels for recognized hot procedure shapes.

| Workload                   |     Java 17 |    Scala 3 |     C# .NET 8 |  F# .NET 8 |     Musi SEAM | Musi vs best peer |
| -------------------------- | ----------: | ---------: | ------------: | ---------: | ------------: | ----------------: |
| `init_small_module`        |   5.4 ns/op |  7.1 ns/op |     8.1 ns/op | 10.1 ns/op | 1,192.3 ns/op |            220.8x |
| `scalar_recursive_sum`     | 434.9 ns/op | 53.2 ns/op | 1,397.4 ns/op | 89.0 ns/op |    64.6 ns/op |              1.2x |
| `closure_capture`          |   5.8 ns/op |  7.6 ns/op |    18.5 ns/op |  5.3 ns/op |    43.1 ns/op |              8.1x |
| `sequence_index_mutation`  |   9.8 ns/op | 22.7 ns/op |    25.0 ns/op | 26.0 ns/op | 1,417.4 ns/op |            144.6x |
| `data_match_option`        |   7.1 ns/op |  7.1 ns/op |     9.3 ns/op |  2.6 ns/op |    42.6 ns/op |             16.4x |
| `effect_resume_equivalent` |   7.2 ns/op |  7.5 ns/op |     9.0 ns/op |  8.5 ns/op | 3,256.7 ns/op |            452.3x |

## Current Musi-Only Baseline

| Workload                    |     Musi SEAM |
| --------------------------- | ------------: |
| `gc_stress_sequence_return` | 2,213.0 ns/op |

## Validation Baseline

| Command                     | Result |
| --------------------------- | ------ |
| `rtk cargo test -p musi_vm` | PASS   |
| `rtk make lint`             | PASS   |
| `rtk make test`             | PASS   |
| `rtk make bench-vms`        | PASS   |
| `rtk make bench-vms-smoke`  | PASS   |

## Garbage Collector Direction

The VM GC is Immix/mark-region.

Current implementation owns observed heap payloads in the VM heap, assigns objects to Immix blocks/lines, traces precise roots, sweeps unreachable records, rebuilds line maps, and breaks unrooted cycles.

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

1. Effect resume: highest remaining semantic overhead; inspect continuation allocation, handler dispatch, and resume value flow without changing handler reuse semantics.
2. Sequence mutation: reduce grid setup cost, heap slot checks, nested sequence lookup, and repeated bounds validation.
3. Initialization: reduce module/closure export setup overhead and repeated heap-root retention.
4. Immix GC: allocation fast path, line/block metadata, root tracing, sweep, and reuse.
5. Tiered kernels: keep scalar recursion, closure capture, and data match fast while expanding only semantics-proven shapes.

## Update Rules

- Update this file only after same-machine runs of `make bench-vm` and `make bench-vms`.
- Record date, commit, toolchain/runtime versions, machine, benchmark estimates, and ratios.
- Keep Musi-only workloads separate from cross-runtime common workloads.

## References

- Cornell CS 6120 Immix summary: <https://www.cs.cornell.edu/courses/cs6120/2019fa/blog/immix/>
- Immix paper seminar mirror: <https://cs.uni-salzburg.at/~ck/content/classes/CMM-Winter-2010/lippautz_immix.pdf>
