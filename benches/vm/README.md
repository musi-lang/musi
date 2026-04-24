# VM Comparison Benchmarks

These harnesses compare Musi's VM running SEAM bytecode with common managed-runtime targets.

Common workloads are intentionally limited to behavior all six targets can express directly:

- Java on JVM (OpenJDK 17)
- Lua 5.5 (PUC-Lua interpreter VM)
- Scala 3 on JVM (Homebrew Scala, Java 17)
- C# on CLR (.NET 8)
- F# on CLR (.NET 8)
- Musi VM over SEAM bytecode (`musi_vm` Criterion bench)

The old C# boxed interpreter baseline was removed because it was not a real runtime peer. These benchmarks compare VM/JIT runtimes instead. Musi-specific workloads stay in the Musi Criterion bench and are reported separately.

Keep comparisons apples-to-apples:

- Peer `native/hot` rows compare with Musi `normal_vm_mode` when the workload shape matches.
- Peer `vm_mode/hot` rows compare with Musi `interpreter_vm_mode` as the fast no-JIT lane.
- Musi `debug_interpreter_vm_mode` is diagnostic-only: no runtime kernels, no fused dispatch.
- Musi `generic_vm_mode` and `hot_vm_mode` are selective optimized paths, not exact peer rows.
- Allocation and GC rows are close-family only because arrays, Lua tables, and Musi sequences do not have the same storage semantics.

Run smoke checks (native + vm_mode lanes):

```sh
make bench-java-smoke
make bench-lua-smoke
make bench-scala-smoke
make bench-csharp-smoke
make bench-fsharp-smoke
```

Run all full baselines (~20m default matrix):

```sh
make bench-vms
```

Run quick local pass (~5m):

```sh
make bench-vms-quick
```

Run long local pass (high-repeat matrix):

```sh
make bench-vms-long
```

Run GC diagnostic lane plus Musi GC Criterion split:

```sh
make bench-vms-gc
```

Run only Musi Criterion numbers:

```sh
make bench-vm
```

Musi Criterion reports six Musi-owned modes:

- `normal_vm_mode`: default tiered VM with reusable bindings and typed kernels where shape is proven
- `generic_vm_mode`: optimized peer-style Musi VM with prewarmed VM, reusable export bindings, runtime kernels, and fused paths
- `interpreter_vm_mode`: fast no-JIT lane; quickening, fused dispatch, and runtime kernels may run
- `debug_interpreter_vm_mode`: diagnostic interpreter lane with kernels and fused dispatch disabled
- `hot_vm_mode`: selective embedder path with bound handles and typed kernels
- `cold_vm_mode`: cold-start diagnostic: load precompiled SEAM bytes, construct VM, initialize, and call once

Musi VM options accepted by `musi run` and `music run`:

- `MVM_OPTIONS="-Xmvm:Tier=Interp -Xmvm:+UseKernels"`
- `-Xmvm:Tier=Normal|Interp|Debug|Hot`
- `-Xmvm:+UseQuickening` / `-Xmvm:-UseQuickening`
- `-Xmvm:+UseKernels` / `-Xmvm:-UseKernels`
- `-Xmvm:+UseFusedDispatch` / `-Xmvm:-UseFusedDispatch`
- `-Xmvm:+UseInlineCaches` / `-Xmvm:-UseInlineCaches`
- `-Xmvm:HeapLimit=<bytes>`
- `-Xmvm:StackLimit=<frames>`
- `-Xmvm:StepLimit=<instructions>`
- `-Xmvm:GC=Auto|Stress`

Environment options are applied before CLI options.

Musi hot `sequence_return_alloc` tracks the bound SEAM hotpath. `sequence_return_call_export_alloc` keeps the general export lookup/call path visible. See `docs/reference/performance.md` for match-level tables.

Harness flags accepted by Java/Lua/Scala/C#/F# runners:

- `--profile native|vm_mode` (`vm_mode` is best-effort no-JIT lane; Lua accepts only `native`)
- `--phase hot|cold|both`
- `--rounds <N>`
- `--iterations <N>`
- `--warmup-iterations <N>`
- `--workload <name|all>`

Output shape:

`runtime/profile/phase/workload -> ns/op`
