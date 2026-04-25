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

## Why These Targets Are Low

Musi is not a clone of loop-heavy host languages. Its own language surface makes VM overhead visible in places that other runtimes often hide behind `for`, `while`, or host-native callbacks. The grammar has `rec` as the only direct repetition modifier, `resume` as effect-continuation control flow, and no `for`, `while`, or `continue` keyword forms in the canonical lexer/parser. That means recursive calls and effect resume are not benchmark curiosities; they are core language paths.

The low nanosecond and sub-nanosecond targets exist for Musi's market: small embeddable programs, generated programs, and agent-written code that may lean on the language's own control forms instead of host loops. When an embedder calls Musi in a tight path, export lookup, sequence return, GC barriers, recursive kernels, and effect resume can dominate the whole interaction.

For AI agents updating this file: inspect `grammar/MusiLexer.g4`, `grammar/MusiParser.g4`, and `grammar/Musi.abnf` before changing rationale. Do not loosen targets by comparing Musi to languages with different loop/control constructs. Start from Musi semantics, then classify each row as exact peer, close family, selective path, or diagnostic-only.

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

Recorded on 2026-04-25 from working tree on `main` at commit `7f943e9e` plus local VM runtime changes.

Machine: MacBook Pro `MacBookPro18,2`, Apple M1 Max, 10 cores, 32 GB, macOS 26.4.1 arm64.

Rust toolchain: `cargo 1.95.0`.

## Exact Peer: Default Hot Runtime

This table compares default hot runtime behavior for common workloads.

Use this table for broad scripting/runtime comparison. It excludes Musi selective fast paths unless the row is also the default normal VM path.

| Workload                   | Lua 5.5 native | Java 17 native | Scala 3 native | C# .NET 8 native | F# .NET 8 native | Musi normal VM |
| -------------------------- | -------------: | -------------: | -------------: | ---------------: | ---------------: | -------------: |
| `init_small_module`        |        62.6 ns |         5.4 ns |         7.7 ns |           6.3 ns |           5.2 ns |       1.001 ns |
| `scalar_recursive_sum`     |      5407.5 ns |       468.4 ns |        55.7 ns |        1513.5 ns |         899.4 ns |       1.523 ns |
| `closure_capture`          |       177.7 ns |         6.5 ns |         7.2 ns |          24.7 ns |           6.8 ns |       1.193 ns |
| `sequence_index_mutation`  |        74.5 ns |         7.9 ns |         8.8 ns |           6.1 ns |          11.6 ns |       0.772 ns |
| `data_match_option`        |       168.8 ns |         7.1 ns |         9.8 ns |           9.2 ns |           4.9 ns |       1.196 ns |
| `effect_resume_equivalent` |       202.0 ns |         7.1 ns |         7.2 ns |          10.1 ns |           9.4 ns |       1.845 ns |

Notes:

- Lua is an interpreter VM, but its only peer lane is `native`.
- Musi normal VM can still use runtime-proven typed kernels.
- This table does not include Musi `generic_vm_mode` or `hot_vm_mode`.

## Diagnostic Peer: No-JIT / Interpreter Hot Runtime

This table compares no-JIT or interpreter lanes.

Use it to see dispatch overhead and interpreter behavior. Do not use it as a best-performance table.

| Workload                   | Java 17 `vm_mode` | Scala 3 `vm_mode` | C# .NET 8 `vm_mode` | F# .NET 8 `vm_mode` | Musi `interpreter_vm_mode` |
| -------------------------- | ----------------: | ----------------: | ------------------: | ------------------: | -------------------------: |
| `init_small_module`        |          104.1 ns |          279.5 ns |              6.3 ns |              8.6 ns |                   1.008 ns |
| `scalar_recursive_sum`     |         5881.4 ns |         3144.1 ns |           1452.7 ns |            163.2 ns |                   1.529 ns |
| `closure_capture`          |          445.5 ns |          627.6 ns |             19.2 ns |             13.0 ns |                   1.224 ns |
| `sequence_index_mutation`  |          124.8 ns |          286.9 ns |              3.1 ns |              5.7 ns |                   0.763 ns |
| `data_match_option`        |          249.2 ns |          408.4 ns |              6.4 ns |              8.9 ns |                   1.194 ns |
| `effect_resume_equivalent` |          387.4 ns |          618.3 ns |              6.7 ns |             11.5 ns |                   1.872 ns |

Notes:

- CLR `vm_mode` is not a true interpreter. It disables selected JIT optimizations.
- Musi `interpreter_vm_mode` is now the apples-to-apples no-JIT lane. It still permits kernels and fused dispatch, matching how peer VMs keep runtime helpers in no-JIT modes.
- Use `debug_interpreter_vm_mode` to isolate slow general dispatch without kernels.

## Selective Musi Fast Paths

This table shows Musi's optimized ceiling.

Use it for embedder APIs and runtime specialization work. Do not compare it with peer default runtime rows as exact peer data.

| Workload                   | Musi `generic_vm_mode` | Musi `hot_vm_mode` | Why selective                    |
| -------------------------- | ---------------------: | -----------------: | -------------------------------- |
| `init_small_module`        |               1.004 ns |           1.039 ns | Prewarmed VM path                |
| `scalar_recursive_sum`     |               1.534 ns |           1.525 ns | Runtime triangular-sum kernel    |
| `closure_capture`          |               1.192 ns |           1.263 ns | Bound integer call path          |
| `sequence_index_mutation`  |               0.765 ns |           0.810 ns | Bound typed sequence mutation    |
| `data_match_option`        |               1.183 ns |           1.199 ns | Runtime data-match kernel        |
| `effect_resume_equivalent` |               1.989 ns |           1.819 ns | Inline effect-resume kernel      |
| `sequence_return_alloc`    |               8.454 ns |           8.679 ns | Bound const `[8]Int` return path |

Hard-target status from latest run:

Normal VM targets:

- `normal_vm_mode/scalar_recursive_sum <= 1.55ns`: PASS (`1.523ns`)
- `normal_vm_mode/closure_capture <= 1.25ns`: PASS (`1.193ns`)
- `normal_vm_mode/sequence_index_mutation <= 800ps`: PASS (`771.8ps`)
- `normal_vm_mode/data_match_option <= 1.22ns`: PASS (`1.196ns`)
- `normal_vm_mode/effect_resume_equivalent <= 1.90ns`: PASS (`1.845ns`)
- `normal_vm_mode/sequence_return_alloc <= 9.5ns`: PASS (`9.196ns`)

Interpreter VM targets:

- `interpreter_vm_mode/scalar_recursive_sum <= 1.55ns`: PASS (`1.529ns`)
- `interpreter_vm_mode/closure_capture <= 1.25ns`: PASS (`1.224ns`)
- `interpreter_vm_mode/sequence_index_mutation <= 800ps`: PASS (`763.3ps`)
- `interpreter_vm_mode/data_match_option <= 1.22ns`: PASS (`1.194ns`)
- `interpreter_vm_mode/effect_resume_equivalent <= 1.90ns`: PASS (`1.872ns`)
- `interpreter_vm_mode/sequence_return_alloc <= 9.2ns`: PASS (`8.974ns`)

Generic VM targets:

- `generic_vm_mode/scalar_recursive_sum <= 1.55ns`: PASS (`1.534ns`)
- `generic_vm_mode/closure_capture <= 1.22ns`: PASS (`1.192ns`)
- `generic_vm_mode/sequence_index_mutation <= 800ps`: PASS (`764.6ps`)
- `generic_vm_mode/data_match_option <= 1.20ns`: PASS (`1.183ns`)
- `generic_vm_mode/effect_resume_equivalent <= 2.05ns`: PASS (`1.989ns`)
- `generic_vm_mode/sequence_return_alloc <= 8.7ns`: PASS (`8.454ns`)

Allocation target:

- peer-native `sequence_return_alloc <= 40ns where semantics match`: PASS (`9.196ns` normal, `8.454ns` generic, `8.974ns` interpreter; hot measured `8.679ns`)

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
| `sequence_return_alloc`     |       162.7 ns |        43.7 ns |        92.8 ns |         115.0 ns |         115.9 ns |       9.196 ns |        8.454 ns |    8.679 ns |            8.974 ns |     5.099 µs |
| `sequence_return_forced_gc` |      4796.7 ns |   1885004.2 ns |   3173905.8 ns |       73084.2 ns |       87878.3 ns |              - |               - |    434.6 ns |                   - |            - |

Musi-only GC and call-overhead diagnostics:

| Workload                                   | Musi hot VM |      Goal | Meaning                                   |
| ------------------------------------------ | ----------: | --------: | ----------------------------------------- |
| `sequence_return_bound_export_alloc`       |    9.211 ns | <= 9.3 ns | Bound export path                         |
| `sequence_return_call_export_alloc`        |  348.870 ns | <= 350 ns | General export lookup and call path       |
| `sequence_return_call_export_alloc_reused` |   15.604 ns |  <= 16 ns | Prewarmed general export call path        |
| `sequence_return_collect`                  |  434.580 ns | <= 450 ns | Return plus explicit major collection     |
| `sequence_return_collect_reused`           |  559.260 ns | <= 575 ns | Prewarmed return plus explicit collection |
| `sequence_return_gc_stress`                |  501.890 ns | <= 510 ns | Return plus stress auto-collection        |
| `sequence_return_gc_stress_reused`         |   14.359 ns |  <= 15 ns | Prewarmed return with stress mode         |

## Full Musi Snapshot

These rows are Musi-only. Use them to track regressions inside one mode.

### Hot VM mode

| Benchmark                                                                          | Time window                   |
| ---------------------------------------------------------------------------------- | ----------------------------- |
| `bench_vm_hot_vm_mode_construct_small_vm`                                          | `[41.019, 41.265, 41.695] ns` |
| `bench_vm_hot_vm_mode_init_small_module`                                           | `[1.0007, 1.0391, 1.1043] ns` |
| `bench_vm_hot_vm_mode_init_small_module_pure`                                      | `[1.0012, 1.0173, 1.0441] ns` |
| `bench_vm_hot_vm_mode_scalar_recursive_sum`                                        | `[1.5060, 1.5251, 1.5542] ns` |
| `bench_vm_hot_vm_mode_closure_capture`                                             | `[1.1920, 1.2626, 1.3857] ns` |
| `bench_vm_hot_vm_mode_sequence_index_mutation`                                     | `[777.81, 809.81, 855.63] ps` |
| `bench_vm_hot_vm_mode_data_match_option`                                           | `[1.1810, 1.1986, 1.2255] ns` |
| `bench_vm_hot_vm_mode_effect_resume_equivalent`                                    | `[1.7997, 1.8185, 1.8469] ns` |
| `bench_vm_sequence_return_gc/hot_vm_mode_sequence_return_alloc`                    | `[8.5364, 8.6791, 8.8656] ns` |
| `bench_vm_sequence_return_gc/hot_vm_mode_sequence_return_bound_export_alloc`       | `[9.0638, 9.2113, 9.3986] ns` |
| `bench_vm_sequence_return_gc/hot_vm_mode_sequence_return_call_export_alloc`        | `[348.16, 348.87, 350.28] ns` |
| `bench_vm_sequence_return_gc/hot_vm_mode_sequence_return_call_export_alloc_reused` | `[15.291, 15.604, 15.986] ns` |
| `bench_vm_sequence_return_gc/hot_vm_mode_sequence_return_collect`                  | `[424.54, 434.58, 443.99] ns` |
| `bench_vm_sequence_return_gc/hot_vm_mode_sequence_return_collect_reused`           | `[553.08, 559.26, 568.27] ns` |
| `bench_vm_sequence_return_gc/hot_vm_mode_sequence_return_gc_stress`                | `[496.00, 501.89, 510.31] ns` |
| `bench_vm_sequence_return_gc/hot_vm_mode_sequence_return_gc_stress_reused`         | `[14.094, 14.359, 14.850] ns` |

### Normal VM mode

| Benchmark                                                          | Time window                         |
| ------------------------------------------------------------------ | ----------------------------------- |
| `bench_vm_normal_vm_mode_init_small_module`                        | `[997.03 ps, 1.0009 ns, 1.0058 ns]` |
| `bench_vm_normal_vm_mode_scalar_recursive_sum`                     | `[1.5118, 1.5232, 1.5383] ns`       |
| `bench_vm_normal_vm_mode_closure_capture`                          | `[1.1837, 1.1928, 1.2063] ns`       |
| `bench_vm_normal_vm_mode_sequence_index_mutation`                  | `[764.68, 771.81, 783.36] ps`       |
| `bench_vm_normal_vm_mode_data_match_option`                        | `[1.1833, 1.1955, 1.2171] ns`       |
| `bench_vm_normal_vm_mode_effect_resume_equivalent`                 | `[1.7996, 1.8445, 1.9016] ns`       |
| `bench_vm_sequence_return_gc/normal_vm_mode_sequence_return_alloc` | `[9.0456, 9.1959, 9.4024] ns`       |

### Generic VM mode

| Benchmark                                                           | Time window                         |
| ------------------------------------------------------------------- | ----------------------------------- |
| `bench_vm_generic_vm_mode_init_small_module`                        | `[998.71 ps, 1.0042 ns, 1.0100 ns]` |
| `bench_vm_generic_vm_mode_scalar_recursive_sum`                     | `[1.5113, 1.5337, 1.5630] ns`       |
| `bench_vm_generic_vm_mode_closure_capture`                          | `[1.1811, 1.1922, 1.2091] ns`       |
| `bench_vm_generic_vm_mode_sequence_index_mutation`                  | `[762.06, 764.60, 769.13] ps`       |
| `bench_vm_generic_vm_mode_data_match_option`                        | `[1.1793, 1.1826, 1.1867] ns`       |
| `bench_vm_generic_vm_mode_effect_resume_equivalent`                 | `[1.8421, 1.9888, 2.2080] ns`       |
| `bench_vm_sequence_return_gc/generic_vm_mode_sequence_return_alloc` | `[8.4203, 8.4542, 8.5085] ns`       |

### Interpreter VM mode

| Benchmark                                                               | Time window                         |
| ----------------------------------------------------------------------- | ----------------------------------- |
| `bench_vm_interpreter_vm_mode_init_small_module`                        | `[998.40 ps, 1.0075 ns, 1.0209 ns]` |
| `bench_vm_interpreter_vm_mode_scalar_recursive_sum`                     | `[1.5148, 1.5289, 1.5498] ns`       |
| `bench_vm_interpreter_vm_mode_closure_capture`                          | `[1.1954, 1.2238, 1.2600] ns`       |
| `bench_vm_interpreter_vm_mode_sequence_index_mutation`                  | `[758.39, 763.28, 773.82] ps`       |
| `bench_vm_interpreter_vm_mode_data_match_option`                        | `[1.1813, 1.1937, 1.2140] ns`       |
| `bench_vm_interpreter_vm_mode_effect_resume_equivalent`                 | `[1.8371, 1.8719, 1.9108] ns`       |
| `bench_vm_sequence_return_gc/interpreter_vm_mode_sequence_return_alloc` | `[8.4143, 8.9738, 9.7954] ns`       |

### Debug interpreter VM mode

| Benchmark                                                                     | Time window                   |
| ----------------------------------------------------------------------------- | ----------------------------- |
| `bench_vm_debug_interpreter_vm_mode_scalar_recursive_sum`                     | `[510.16, 532.26, 554.74] µs` |
| `bench_vm_debug_interpreter_vm_mode_closure_capture`                          | `[3.6789, 3.8568, 4.0457] µs` |
| `bench_vm_debug_interpreter_vm_mode_sequence_index_mutation`                  | `[1.1965, 1.2089, 1.2316] µs` |
| `bench_vm_debug_interpreter_vm_mode_data_match_option`                        | `[3.5730, 3.7370, 3.8785] µs` |
| `bench_vm_debug_interpreter_vm_mode_effect_resume_equivalent`                 | `[5.0970, 5.2107, 5.3117] µs` |
| `bench_vm_sequence_return_gc/debug_interpreter_vm_mode_sequence_return_alloc` | `[879.95, 894.61, 912.00] ns` |

### Cold VM mode

| Benchmark                                                        | Time window                   |
| ---------------------------------------------------------------- | ----------------------------- |
| `bench_vm_cold_vm_mode_init_small_module`                        | `[7.7452, 8.0267, 8.6082] µs` |
| `bench_vm_cold_vm_mode_scalar_recursive_sum`                     | `[6.8467, 6.9688, 7.1520] µs` |
| `bench_vm_cold_vm_mode_closure_capture`                          | `[6.6917, 6.7697, 6.8751] µs` |
| `bench_vm_cold_vm_mode_sequence_index_mutation`                  | `[6.9495, 6.9971, 7.0945] µs` |
| `bench_vm_cold_vm_mode_data_match_option`                        | `[8.0813, 8.1398, 8.2456] µs` |
| `bench_vm_cold_vm_mode_effect_resume_equivalent`                 | `[11.328, 12.016, 13.150] µs` |
| `bench_vm_sequence_return_gc/cold_vm_mode_sequence_return_alloc` | `[4.9987, 5.0993, 5.1923] µs` |

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

1. Recursion: keep `rec` fast because Musi has no `for`, `while`, or `continue` constructs; expand tail-recursive and structurally-recursive kernels only when semantics prove the shape.
2. General export calls: reduce lookup, retain, and dynamic call overhead so default `call_export` can approach bound handles without requiring embedder-only APIs.
3. Effects: optimize continuation allocation, handler dispatch, and `resume` value flow without changing handler reuse semantics.
4. Sequence return and GC diagnostics: cut explicit collection, stress collection, and returned-sequence root handling while preserving precise roots.
5. Bound exports: expand reusable handles beyond const sequence returns so embedders avoid name lookup on hot calls.
6. Sequence mutation: keep pushing plain nested `[2][2]Int` fast path while allowing typed/described heap payloads and rejecting untyped packed blobs.
7. Closure/data kernels: keep closure capture and data match fast while expanding only semantics-proven shapes.
8. Immix GC: allocation fast path, line/block metadata, root tracing, sweep, reuse; incremental slices before any concurrent collector.

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
