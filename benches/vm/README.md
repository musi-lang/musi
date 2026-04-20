# VM Comparison Benchmarks

These harnesses compare Musi's SEAM VM with common managed-runtime targets.

Common workloads are intentionally limited to behavior all five targets can express directly:

- Java on JVM (OpenJDK 17)
- Scala 3 on JVM (Homebrew Scala, Java 17)
- C# on CLR (.NET 8)
- F# on CLR (.NET 8)
- Musi on SEAM (`musi_vm` Criterion bench)

The old C# boxed interpreter baseline was removed because it was not a real runtime peer. These benchmarks compare VM/JIT runtimes instead. Musi-specific workloads stay in the Musi Criterion bench and are reported separately.

Run smoke checks (native + vm_mode lanes):

```sh
make bench-java-smoke
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

Run only Musi Criterion numbers:

```sh
make bench-vm
```

Harness flags accepted by Java/Scala/C#/F# runners:

- `--profile native|vm_mode` (`vm_mode` is best-effort no-JIT lane)
- `--phase hot|cold|both`
- `--rounds <N>`
- `--iterations <N>`
- `--warmup-iterations <N>`

Output shape:

`runtime/profile/phase/workload -> ns/op`
