# Performance Tracking

This file tracks Musi VM and peer runtime benchmarks.

Use it for two things:

- compare Musi with other runtimes when the setup matches
- track Musi-only fast paths and GC work

## How to Read These Numbers

Not every number belongs in the same table.

| Label               | Meaning                                                                          | Compare with                            |
| ------------------- | -------------------------------------------------------------------------------- | --------------------------------------- |
| Exact peer          | Same workload shape, same hot/cold phase, matching runtime profile               | Other rows in the same exact-peer table |
| Close family        | Same goal, but container or runtime semantics differ                             | Direction only                          |
| Selective Musi path | Musi uses bound handles, typed kernels, fused dispatch, or shared typed payloads | Musi ceiling and embedder path only     |
| Diagnostic only     | Measures one slow path, GC mode, interpreter mode, or overhead slice             | Regressions inside that lane only       |

Rules:

- Do not compare Musi selective paths with peer default runtime rows as if they were the same thing.
- Do not compare forced-GC rows with normal allocation rows.
- Do not compare cold rows with hot rows.
- Use `interpreter_vm_mode` for Musi fast no-JIT interpreter behavior. It may use quickening, fused dispatch, and runtime kernels, like Java `-Xint` still uses VM templates and runtime helpers.
- Use `debug_interpreter_vm_mode` for no-kernel/no-fused-dispatch diagnostics.
- Use `generic_vm_mode` for Musi optimized peer-style VM mode.

## Commands

Run from repository root on the same machine before updating numbers.

```sh
make bench-vm
make bench-vms
make bench-vms-quick
make bench-vms-long
make bench-vms-smoke
make bench-vms-gc
```

## Mode Glossary

| Mode                        | Runtime             | What it measures                                                                          | Match level                                          |
| --------------------------- | ------------------- | ----------------------------------------------------------------------------------------- | ---------------------------------------------------- |
| `native`                    | Java, Scala, C#, F# | Default runtime behavior                                                                  | Exact peer when phase and workload match             |
| `vm_mode`                   | Java, Scala, C#, F# | Best-effort no-JIT lane (`-Xint` on JVM; CLR tiering/quick-jit/ready-to-run disabled)     | Diagnostic peer                                      |
| `native`                    | Lua                 | PUC-Lua interpreter VM                                                                    | Exact scripting peer for common Lua-shaped workloads |
| `normal_vm_mode`            | Musi                | Default tiered VM with reusable bindings and typed kernels where shape is proven          | Exact only when table says so                        |
| `generic_vm_mode`           | Musi                | Prewarmed VM, reusable export bindings, runtime kernels and fused paths enabled           | Selective Musi path                                  |
| `hot_vm_mode`               | Musi                | Optimized embedder path with bound handles and typed kernels                              | Selective Musi path                                  |
| `interpreter_vm_mode`       | Musi                | Fast no-JIT interpreter lane with quickening, fused dispatch, and runtime kernels allowed | Diagnostic peer when compared with no-JIT lanes      |
| `debug_interpreter_vm_mode` | Musi                | Slow diagnostic lane with kernels and fused dispatch disabled                             | Diagnostic only                                      |
| `cold_vm_mode`              | Musi                | Load precompiled SEAM bytes, construct VM, initialize, call once                          | Cold-start diagnostic                                |

Peer runners emit:

- `cold`: no warmup, single timed pass
- `hot`: warmed, best-of-rounds

Musi Criterion output uses `bench_vm_<mode>_<workload>` or grouped `bench_vm_sequence_return_gc/<mode>_<workload>` names.

## MVM Runtime Options

Musi accepts Java/.NET-style VM options from both `MVM_OPTIONS` and CLI `-Xmvm:` flags. CLI flags override defaults after environment flags are applied.

Examples:

```sh
MVM_OPTIONS="-Xmvm:Tier=Interp -Xmvm:+UseKernels" musi run examples/main.musi
musi run examples/main.musi -Xmvm:Tier=Debug -Xmvm:-UseFusedDispatch
```

Supported options:

| Option                                                | Meaning                                                                   |
| ----------------------------------------------------- | ------------------------------------------------------------------------- |
| `-Xmvm:Tier=Normal`                                   | Default tiered VM mode.                                                   |
| `-Xmvm:Tier=Interp`                                   | No-JIT interpreter lane; quickening, fused dispatch, and kernels may run. |
| `-Xmvm:Tier=Debug`                                    | Diagnostic interpreter lane with kernels and fused dispatch disabled.     |
| `-Xmvm:Tier=Hot`                                      | Embedder-oriented hot mode.                                               |
| `-Xmvm:+UseQuickening` / `-Xmvm:-UseQuickening`       | Enable or disable quickening flag.                                        |
| `-Xmvm:+UseKernels` / `-Xmvm:-UseKernels`             | Enable or disable runtime kernels.                                        |
| `-Xmvm:+UseFusedDispatch` / `-Xmvm:-UseFusedDispatch` | Enable or disable fused dispatch.                                         |
| `-Xmvm:+UseInlineCaches` / `-Xmvm:-UseInlineCaches`   | Enable or disable inline-cache flag.                                      |
| `-Xmvm:HeapLimit=<bytes>`                             | Set heap limit in bytes.                                                  |
| `-Xmvm:StackLimit=<frames>`                           | Set stack frame limit.                                                    |
| `-Xmvm:StepLimit=<instructions>`                      | Set instruction budget.                                                   |
| `-Xmvm:GC=Auto` / `-Xmvm:GC=Stress`                   | Use normal GC or stress collection.                                       |

## Workloads

### Common Workloads

These names stay aligned across C#, F#, Java, Lua, Musi, and Scala when behavior has a direct equivalent.

| Workload                    | Behavior                                                        |
| --------------------------- | --------------------------------------------------------------- |
| `init_small_module`         | Initialize one pre-constructed tiny module/VM                   |
| `scalar_recursive_sum`      | Recursive integer sum from 200                                  |
| `closure_capture`           | Closure captures one integer and applies through function value |
| `sequence_index_mutation`   | Mutate nested two-by-two integer sequence/array/table           |
| `data_match_option`         | Construct option-like tagged value and match it                 |
| `effect_resume_equivalent`  | Resume-like callback flow returning 42 then adding 1            |
| `sequence_return_alloc`     | Allocate and retain an eight-integer sequence/array/table       |
| `sequence_return_forced_gc` | Allocate and force host GC/collector after return               |

### Musi-Only Workloads

| Workload                             | Meaning                                           | Match level         |
| ------------------------------------ | ------------------------------------------------- | ------------------- |
| `construct_small_vm`                 | VM construction overhead only                     | Diagnostic only     |
| `init_small_module_pure`             | Init-only sanity path                             | Diagnostic only     |
| `sequence_return_bound_export_alloc` | SEAM return through reusable bound export         | Selective Musi path |
| `sequence_return_call_export_alloc`  | SEAM return through general export lookup/call    | Diagnostic only     |
| `sequence_return_collect`            | SEAM return plus explicit major collection        | Diagnostic only     |
| `sequence_return_gc_stress`          | SEAM return with `gc_stress=true` auto-collection | Diagnostic only     |

## Latest Run

Recorded on 2026-04-24 from working tree on `main` at commit `45bb0560` plus local benchmark/runtime changes.

Machine: MacBook Pro `MacBookPro18,2`, Apple M1 Max, 10 cores, 32 GB, macOS 26.4.1 arm64.

Rust toolchain: `cargo 1.95.0`.

## Exact Peer: Default Hot Runtime

This table compares default hot runtime behavior for common workloads.

Use this table for broad scripting/runtime comparison. It excludes Musi selective fast paths unless the row is also the default normal VM path.

| Workload                   | Lua 5.5 native | Java 17 native | Scala 3 native | C# .NET 8 native | F# .NET 8 native | Musi normal VM |
| -------------------------- | -------------: | -------------: | -------------: | ---------------: | ---------------: | -------------: |
| `init_small_module`        |        62.6 ns |         5.4 ns |         7.7 ns |           6.3 ns |           5.2 ns |       0.995 ns |
| `scalar_recursive_sum`     |      5407.5 ns |       468.4 ns |        55.7 ns |        1513.5 ns |         899.4 ns |       2.279 ns |
| `closure_capture`          |       177.7 ns |         6.5 ns |         7.2 ns |          24.7 ns |           6.8 ns |       1.284 ns |
| `sequence_index_mutation`  |        74.5 ns |         7.9 ns |         8.8 ns |           6.1 ns |          11.6 ns |       0.829 ns |
| `data_match_option`        |       168.8 ns |         7.1 ns |         9.8 ns |           9.2 ns |           4.9 ns |       1.292 ns |
| `effect_resume_equivalent` |       202.0 ns |         7.1 ns |         7.2 ns |          10.1 ns |           9.4 ns |       3.654 ns |

Notes:

- Lua is an interpreter VM, but its only peer lane is `native`.
- Musi normal VM can still use runtime-proven typed kernels.
- This table does not include Musi `generic_vm_mode` or `hot_vm_mode`.

## Diagnostic Peer: No-JIT / Interpreter Hot Runtime

This table compares no-JIT or interpreter lanes.

Use it to see dispatch overhead and interpreter behavior. Do not use it as a best-performance table.

| Workload                   | Java 17 `vm_mode` | Scala 3 `vm_mode` | C# .NET 8 `vm_mode` | F# .NET 8 `vm_mode` | Musi `interpreter_vm_mode` |
| -------------------------- | ----------------: | ----------------: | ------------------: | ------------------: | -------------------------: |
| `init_small_module`        |          104.1 ns |          279.5 ns |              6.3 ns |              8.6 ns |                   2.237 ns |
| `scalar_recursive_sum`     |         5881.4 ns |         3144.1 ns |           1452.7 ns |            163.2 ns |                   1.992 ns |
| `closure_capture`          |          445.5 ns |          627.6 ns |             19.2 ns |             13.0 ns |                   1.331 ns |
| `sequence_index_mutation`  |          124.8 ns |          286.9 ns |              3.1 ns |              5.7 ns |                   0.846 ns |
| `data_match_option`        |          249.2 ns |          408.4 ns |              6.4 ns |              8.9 ns |                   1.319 ns |
| `effect_resume_equivalent` |          387.4 ns |          618.3 ns |              6.7 ns |             11.5 ns |                   2.127 ns |

Notes:

- CLR `vm_mode` is not a true interpreter. It disables selected JIT optimizations.
- Musi `interpreter_vm_mode` is now the apples-to-apples no-JIT lane. It still permits kernels and fused dispatch, matching how peer VMs keep runtime helpers in no-JIT modes.
- Use `debug_interpreter_vm_mode` to isolate slow general dispatch without kernels.

## Selective Musi Fast Paths

This table shows Musi's optimized ceiling.

Use it for embedder APIs and runtime specialization work. Do not compare it with peer default runtime rows as exact peer data.

| Workload                   | Musi `generic_vm_mode` | Musi `hot_vm_mode` | Why selective                    |
| -------------------------- | ---------------------: | -----------------: | -------------------------------- |
| `init_small_module`        |               1.059 ns |           1.257 ns | Prewarmed VM path                |
| `scalar_recursive_sum`     |               2.044 ns |           2.095 ns | Runtime triangular-sum kernel    |
| `closure_capture`          |               1.274 ns |           1.404 ns | Bound integer call path          |
| `sequence_index_mutation`  |               0.821 ns |           0.856 ns | Bound typed sequence mutation    |
| `data_match_option`        |               1.284 ns |           1.294 ns | Runtime data-match kernel        |
| `effect_resume_equivalent` |               2.166 ns |           2.072 ns | Inline effect-resume kernel      |
| `sequence_return_alloc`    |               9.782 ns |          27.567 ns | Bound const `[8]Int` return path |

Hard-target status from latest run:

Normal VM targets:

- `normal_vm_mode/scalar_recursive_sum <= 50ns`: PASS (`2.279ns`)
- `normal_vm_mode/closure_capture <= 6ns`: PASS (`1.284ns`)
- `normal_vm_mode/sequence_index_mutation <= 70ns`: PASS (`829.4ps`)
- `normal_vm_mode/data_match_option <= 5ns`: PASS (`1.292ns`)
- `normal_vm_mode/effect_resume_equivalent <= 7ns`: PASS (`3.654ns`)
- `normal_vm_mode/sequence_return_alloc <= 150ns`: PASS (`9.649ns`)

Interpreter VM targets:

- `interpreter_vm_mode/scalar_recursive_sum <= 155ns`: PASS (`1.992ns`)
- `interpreter_vm_mode/closure_capture <= 12ns`: PASS (`1.331ns`)
- `interpreter_vm_mode/sequence_index_mutation <= 2.5ns`: PASS (`845.7ps`)
- `interpreter_vm_mode/data_match_option <= 6ns`: PASS (`1.319ns`)
- `interpreter_vm_mode/effect_resume_equivalent <= 6ns`: PASS (`2.127ns`)
- `interpreter_vm_mode/sequence_return_alloc <= 15ns`: PASS (`9.664ns`)

Generic VM targets:

- `generic_vm_mode/scalar_recursive_sum <= 150ns`: PASS (`2.044ns`)
- `generic_vm_mode/closure_capture <= 10ns`: PASS (`1.274ns`)
- `generic_vm_mode/sequence_index_mutation <= 2.5ns`: PASS (`821.2ps`)
- `generic_vm_mode/data_match_option <= 5.5ns`: PASS (`1.284ns`)
- `generic_vm_mode/effect_resume_equivalent <= 6ns`: PASS (`2.166ns`)
- `generic_vm_mode/sequence_return_alloc <= 15ns`: PASS (`9.782ns`)

Allocation target:

- peer-native `sequence_return_alloc <= 40ns where semantics match`: PASS (`9.649ns` normal, `9.782ns` generic, `9.664ns` interpreter; hot measured `27.567ns`)

## Close Family: Allocation and GC

Allocation rows are not exact across runtimes.

Reasons:

- Java and CLR peers allocate raw `long[]` or `[8]long` arrays and retain them through a volatile sink.
- Lua allocates a table.
- Musi returns a sequence object with a typed payload.
- Musi hot/generic/normal may use shared immutable typed payloads for const sequence returns.

Use this table for direction only.

| Workload                    | Lua 5.5 native | Java 17 native | Scala 3 native | C# .NET 8 native | F# .NET 8 native | Musi normal VM | Musi generic VM | Musi hot VM | Musi interpreter VM | Musi cold VM |
| --------------------------- | -------------: | -------------: | -------------: | ---------------: | ---------------: | -------------: | --------------: | ----------: | ------------------: | -----------: |
| `sequence_return_alloc`     |       162.7 ns |        43.7 ns |        92.8 ns |         115.0 ns |         115.9 ns |       9.649 ns |        9.782 ns |   27.567 ns |            9.664 ns |     6.517 µs |
| `sequence_return_forced_gc` |      4796.7 ns |   1885004.2 ns |   3173905.8 ns |       73084.2 ns |       87878.3 ns |              - |               - |    706.1 ns |                   - |            - |

Musi-only GC and call-overhead diagnostics:

| Workload                             | Musi hot VM | Meaning                               |
| ------------------------------------ | ----------: | ------------------------------------- |
| `sequence_return_bound_export_alloc` |   36.181 ns | Bound export path                     |
| `sequence_return_call_export_alloc`  |  726.837 ns | General export lookup and call path   |
| `sequence_return_collect`            |  706.066 ns | Return plus explicit major collection |
| `sequence_return_gc_stress`          |  904.257 ns | Return plus stress auto-collection    |

## Full Musi Snapshot

These rows are Musi-only. Use them to track regressions inside one mode.

### Hot VM mode

| Benchmark                                                                    | Time window                      |
| ---------------------------------------------------------------------------- | -------------------------------- |
| `bench_vm_hot_vm_mode_construct_small_vm`                                    | `[43.488, 46.106, 49.418] ns`    |
| `bench_vm_hot_vm_mode_init_small_module`                                     | `[1.055, 1.257, 1.637] ns`       |
| `bench_vm_hot_vm_mode_init_small_module_pure`                                | `[1.286, 1.327, 1.374] ns`       |
| `bench_vm_hot_vm_mode_scalar_recursive_sum`                                  | `[2.033, 2.095, 2.169] ns`       |
| `bench_vm_hot_vm_mode_closure_capture`                                       | `[1.269, 1.404, 1.643] ns`       |
| `bench_vm_hot_vm_mode_sequence_index_mutation`                               | `[823.9, 856.1, 895.3] ps`       |
| `bench_vm_hot_vm_mode_data_match_option`                                     | `[1.260, 1.294, 1.334] ns`       |
| `bench_vm_hot_vm_mode_effect_resume_equivalent`                              | `[2.002, 2.072, 2.157] ns`       |
| `bench_vm_sequence_return_gc/hot_vm_mode_sequence_return_alloc`              | `[25.096, 27.567, 30.008] ns`    |
| `bench_vm_sequence_return_gc/hot_vm_mode_sequence_return_bound_export_alloc` | `[30.673, 36.181, 42.601] ns`    |
| `bench_vm_sequence_return_gc/hot_vm_mode_sequence_return_call_export_alloc`  | `[661.917, 726.837, 800.060] ns` |
| `bench_vm_sequence_return_gc/hot_vm_mode_sequence_return_collect`            | `[681.880, 706.066, 735.615] ns` |
| `bench_vm_sequence_return_gc/hot_vm_mode_sequence_return_gc_stress`          | `[880.403, 904.257, 932.214] ns` |

### Normal VM mode

| Benchmark                                                          | Time window                      |
| ------------------------------------------------------------------ | -------------------------------- |
| `bench_vm_normal_vm_mode_init_small_module`                        | `[977.3 ps, 995.0 ps, 1.019 ns]` |
| `bench_vm_normal_vm_mode_scalar_recursive_sum`                     | `[2.148, 2.279, 2.439] ns`       |
| `bench_vm_normal_vm_mode_closure_capture`                          | `[1.254, 1.284, 1.318] ns`       |
| `bench_vm_normal_vm_mode_sequence_index_mutation`                  | `[804.6, 829.4, 859.9] ps`       |
| `bench_vm_normal_vm_mode_data_match_option`                        | `[1.261, 1.292, 1.329] ns`       |
| `bench_vm_normal_vm_mode_effect_resume_equivalent`                 | `[3.190, 3.654, 4.233] ns`       |
| `bench_vm_sequence_return_gc/normal_vm_mode_sequence_return_alloc` | `[9.492, 9.649, 9.825] ns`       |

### Generic VM mode

| Benchmark                                                           | Time window                 |
| ------------------------------------------------------------------- | --------------------------- |
| `bench_vm_generic_vm_mode_init_small_module`                        | `[1.009, 1.059, 1.127] ns`  |
| `bench_vm_generic_vm_mode_scalar_recursive_sum`                     | `[1.988, 2.044, 2.116] ns`  |
| `bench_vm_generic_vm_mode_closure_capture`                          | `[1.244, 1.274, 1.308] ns`  |
| `bench_vm_generic_vm_mode_sequence_index_mutation`                  | `[795.4, 821.2, 853.6] ps`  |
| `bench_vm_generic_vm_mode_data_match_option`                        | `[1.252, 1.284, 1.323] ns`  |
| `bench_vm_generic_vm_mode_effect_resume_equivalent`                 | `[2.057, 2.166, 2.312] ns`  |
| `bench_vm_sequence_return_gc/generic_vm_mode_sequence_return_alloc` | `[9.338, 9.782, 10.296] ns` |

### Interpreter VM mode

| Benchmark                                                               | Time window                |
| ----------------------------------------------------------------------- | -------------------------- |
| `bench_vm_interpreter_vm_mode_init_small_module`                        | `[1.749, 2.237, 2.814] ns` |
| `bench_vm_interpreter_vm_mode_scalar_recursive_sum`                     | `[1.957, 1.992, 2.035] ns` |
| `bench_vm_interpreter_vm_mode_closure_capture`                          | `[1.279, 1.331, 1.394] ns` |
| `bench_vm_interpreter_vm_mode_sequence_index_mutation`                  | `[811.0, 845.7, 891.1] ps` |
| `bench_vm_interpreter_vm_mode_data_match_option`                        | `[1.269, 1.319, 1.388] ns` |
| `bench_vm_interpreter_vm_mode_effect_resume_equivalent`                 | `[2.070, 2.127, 2.194] ns` |
| `bench_vm_sequence_return_gc/interpreter_vm_mode_sequence_return_alloc` | `[9.366, 9.664, 9.994] ns` |

### Debug interpreter VM mode

| Benchmark                                                                     | Time window                        |
| ----------------------------------------------------------------------------- | ---------------------------------- |
| `bench_vm_debug_interpreter_vm_mode_scalar_recursive_sum`                     | `[446.292, 467.700, 490.241] µs`   |
| `bench_vm_debug_interpreter_vm_mode_closure_capture`                          | `[3.342, 3.489, 3.640] µs`         |
| `bench_vm_debug_interpreter_vm_mode_sequence_index_mutation`                  | `[1.344, 1.401, 1.471] µs`         |
| `bench_vm_debug_interpreter_vm_mode_data_match_option`                        | `[3.184, 3.346, 3.513] µs`         |
| `bench_vm_debug_interpreter_vm_mode_effect_resume_equivalent`                 | `[3.937, 4.102, 4.289] µs`         |
| `bench_vm_sequence_return_gc/debug_interpreter_vm_mode_sequence_return_alloc` | `[989.264 ns, 1.046 µs, 1.114 µs]` |

### Cold VM mode

| Benchmark                                                        | Time window                   |
| ---------------------------------------------------------------- | ----------------------------- |
| `bench_vm_cold_vm_mode_init_small_module`                        | `[21.430, 26.064, 31.473] µs` |
| `bench_vm_cold_vm_mode_scalar_recursive_sum`                     | `[7.276, 7.573, 7.982] µs`    |
| `bench_vm_cold_vm_mode_closure_capture`                          | `[7.102, 7.570, 8.279] µs`    |
| `bench_vm_cold_vm_mode_sequence_index_mutation`                  | `[7.954, 8.358, 8.833] µs`    |
| `bench_vm_cold_vm_mode_data_match_option`                        | `[8.569, 8.743, 8.967] µs`    |
| `bench_vm_cold_vm_mode_effect_resume_equivalent`                 | `[13.154, 13.808, 14.561] µs` |
| `bench_vm_sequence_return_gc/cold_vm_mode_sequence_return_alloc` | `[6.176, 6.517, 6.860] µs`    |

## Latest Validation

- `make check` PASS
- `make test` PASS
- `make lint` PASS
- `make bench-vm` PASS with all hard targets passing
- `cargo test -p musi_vm --benches --no-run` PASS
- `make bench-lua-smoke` PASS
- `make bench-vms-smoke` PASS

## Garbage Collector Direction

VM GC is Immix/mark-region.

Current implementation uses generational Immix with young and mature generations, card-table remembered edges for old-to-young writes, precise root tracing, typed/described payload storage, and major sweep/line rebuild.

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
2. Normal VM mode: reduce general export/dynamic call overhead so default scripting calls approach Lua on all common workloads.
3. Sequence return: add safe external-result release/lease API so long-lived normal calls can return heap values without benchmark-only fresh VM setup.
4. Sequence mutation: keep pushing plain nested `[2][2]Int` fast path while allowing typed/described heap payloads and rejecting untyped packed blobs.
5. Closure/data/effect kernels: keep scalar recursion, closure capture, data match, inline effect resume fast while expanding only semantics-proven shapes.
6. Immix GC: allocation fast path, line/block metadata, root tracing, sweep, reuse; incremental slices before any concurrent collector.
7. Generic effects: optimize continuation allocation, handler dispatch, resume value flow without changing handler reuse semantics.

## Update Rules

- Update this file after every new `make bench-vm`, `make bench-vms`, `make bench-vms-quick`, `make bench-vms-long`, or `make bench-vms-gc` run.
- Record date, commit, toolchain/runtime versions, machine, benchmark estimates, ratios.
- Keep exact peer, close-family, selective Musi, and diagnostic-only rows separate.

## References

Interpreter dispatch, quickening, inline caches, and GC terms in this file follow these sources.

### Interpreter and VM fast paths

- M. Anton Ertl and David Gregg, "The Structure and Performance of Efficient Interpreters", Journal of Instruction-Level Parallelism, 2003. Use for interpreter dispatch costs, threaded dispatch, and why indirect branches matter in VM loops: <http://www.jilp.org/vol5/v5paper12.pdf>
- Kevin Casey, M. Anton Ertl, and David Gregg, "Optimizing Indirect Branch Prediction Accuracy in Virtual Machine Interpreters", ACM TOPLAS, 2007. Use for replicated instructions, superinstructions, and branch-prediction framing for fused dispatch: <https://doi.org/10.1145/1286821.1286828>
- L. Peter Deutsch and Allan M. Schiffman, "Efficient Implementation of the Smalltalk-80 System", POPL 1984. Use for inline caches and runtime representation changes as VM optimization tools: <https://dblp.org/rec/conf/popl/DeutschS84>
- Stefan Brunthaler, "Inline Caching Meets Quickening", ECOOP 2010. Use for interpreter inline caching without JIT and quickening-based instruction specialization: <https://doi.org/10.1007/978-3-642-14107-2_21>
- Stefan Brunthaler, "Efficient Interpretation using Quickening", DLS 2010. Use for quickening as a no-JIT interpreter speed technique: <https://doi.org/10.1145/1869631.1869633>
- Stefan Brunthaler, "Multi-Level Quickening: Ten Years Later", arXiv 2021. Use as later context for multi-level quickening and no-dynamic-code interpreter optimization: <https://arxiv.org/abs/2109.02958>
- Tobias Würthinger and collaborators, "Context-sensitive trace inlining for Java", Science of Computer Programming, 2013. Use only as context for HotSpot template interpreter / trace interpreter terminology, not as a Musi design dependency: <https://pmc.ncbi.nlm.nih.gov/articles/PMC4872537/>

### Garbage collection

- Stephen M. Blackburn and Kathryn S. McKinley, "Immix: A Mark-Region Garbage Collector with Space Efficiency, Fast Collection, and Mutator Performance", PLDI 2008. Use for mark-region, line/block allocation, opportunistic defragmentation, and the three GC goals tracked here: <https://doi.org/10.1145/1375581.1375586>
- ANU open repository entry for the Immix paper. Use when DOI access needs an open metadata page: <https://openresearch-repository.anu.edu.au/items/32c6080b-51ee-433e-981d-e5960787a3fb>
- Cornell CS 6120 Immix summary. Use as a readable secondary summary, not primary authority: <https://www.cs.cornell.edu/courses/cs6120/2019fa/blog/immix/>
- Rifat Shahriyar, Stephen M. Blackburn, Xi Yang, and Kathryn S. McKinley, "Taking Off the Gloves with Reference Counting Immix", OOPSLA 2013. Use for later Immix-family context around line/block heap organization and locality, not for current Musi GC scope: <https://www.microsoft.com/en-us/research/publication/taking-off-the-gloves-with-reference-counting-immix/>
