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
| `init_small_module`        | initialize one pre-constructed VM for tiny module               |
| `scalar_recursive_sum`     | recursive integer sum from 200                                  |
| `closure_capture`          | closure captures one integer and applies through function value |
| `sequence_index_mutation`  | mutate nested two-by-two integer sequence                       |
| `data_match_option`        | construct option-like tagged value and match it                 |
| `effect_resume_equivalent` | resume-like callback flow returning 42 then adding 1            |

## Musi-Only Workloads

These stay outside cross-runtime comparison unless an honest behavioral equivalent exists:

| Workload                    | Reason                             |
| --------------------------- | ---------------------------------- |
| `construct_small_vm`        | VM construction overhead only      |
| `gc_stress_sequence_return` | SEAM heap/Immix collector behavior |

## Current Baseline

Recorded on 2026-04-20 from the VM performance working tree based on `aaa15eeb` on MacBook Pro `MacBookPro18,2`, Apple M1 Max, 10 cores, 32 GB, macOS 26.4.1 arm64. Rust used `cargo 1.95.0`; Java used OpenJDK `17.0.18`; Scala used Scala CLI `1.13.0`; .NET SDK was `10.0.202`; CLR runtime reported by the benches was `8.0.26`.

Do not mix historical C# interpreter data with this multi-VM table. Values are point estimates from a full local run. Musi SEAM values are Criterion mean point estimates from `estimates.json`. Lower is better. Musi currently uses SEAM bytecode plus tiered runtime kernels for recognized hot procedure shapes. `init_small_module` now measures initialize-phase cost only; VM construction moved to Musi-only `construct_small_vm`.

| Workload                   |     Java 17 |    Scala 3 |     C# .NET 8 |  F# .NET 8 |   Musi SEAM | Musi vs best peer |
| -------------------------- | ----------: | ---------: | ------------: | ---------: | ----------: | ----------------: |
| `init_small_module`        |   4.6 ns/op |  6.3 ns/op |     6.9 ns/op |  8.7 ns/op |  90.7 ns/op |             19.7x |
| `scalar_recursive_sum`     | 376.4 ns/op | 45.7 ns/op | 1,205.6 ns/op | 78.2 ns/op |  32.1 ns/op |              0.7x |
| `closure_capture`          |   5.2 ns/op |  6.1 ns/op |    15.7 ns/op |  4.7 ns/op |  16.4 ns/op |              3.5x |
| `sequence_index_mutation`  |   8.3 ns/op | 16.3 ns/op |    23.3 ns/op | 22.4 ns/op | 278.4 ns/op |             33.5x |
| `data_match_option`        |   6.1 ns/op |  6.1 ns/op |     7.9 ns/op |  2.2 ns/op |  16.5 ns/op |              7.5x |
| `effect_resume_equivalent` |   6.3 ns/op |  6.1 ns/op |     7.8 ns/op |  7.2 ns/op |  13.7 ns/op |              2.2x |

## Current Musi-Only Baseline

| Workload                    |     Musi SEAM |
| --------------------------- | ------------: |
| `construct_small_vm`        |    54.8 ns/op |
| `gc_stress_sequence_return` | 1,301.9 ns/op |

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

Current implementation uses generational Immix (young+mature), card-table remembered edges for old->young writes, precise root tracing, and major sweep/line rebuild.

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

1. Sequence mutation: reduce grid setup cost, heap slot checks, nested sequence lookup, and repeated bounds validation.
2. Initialization: reduce module/closure export setup overhead and repeated heap-root retention.
3. Closure/data/effect kernels: keep scalar recursion, closure capture, data match, and inline effect resume fast while expanding only semantics-proven shapes.
4. Immix GC: allocation fast path, line/block metadata, root tracing, sweep, and reuse.
5. Generic effects: optimize continuation allocation, handler dispatch, and resume value flow without changing handler reuse semantics.

## Update Rules

- Update this file after every new `make bench-vm` or `make bench-vms` run; do not leave fresh bench values only in chat or logs.
- Record date, commit, toolchain/runtime versions, machine, benchmark estimates, and ratios.
- Keep Musi-only workloads separate from cross-runtime common workloads.

## References

- Cornell CS 6120 Immix summary: <https://www.cs.cornell.edu/courses/cs6120/2019fa/blog/immix/>
- Immix paper seminar mirror: <https://cs.uni-salzburg.at/~ck/content/classes/CMM-Winter-2010/lippautz_immix.pdf>
