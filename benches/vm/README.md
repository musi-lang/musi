# VM Comparison Benchmarks

These harnesses compare Musi's SEAM VM with common managed-runtime targets.

Common workloads are intentionally limited to behavior all five targets can express directly:

- Java on JVM (OpenJDK 17)
- Scala 3 on JVM (Homebrew Scala, Java 17)
- C# on CLR (.NET 8)
- F# on CLR (.NET 8)
- Musi on SEAM (`musi_vm` Criterion bench)

The old C# boxed interpreter baseline was removed because it was not a real runtime peer. These benchmarks compare VM/JIT runtimes instead. Musi-specific workloads stay in the Musi Criterion bench and are reported separately.

Run smoke checks:

```sh
make bench-java-smoke
make bench-scala-smoke
make bench-csharp-smoke
make bench-fsharp-smoke
```

Run all full baselines:

```sh
make bench-vms
```

Run only Musi Criterion numbers:

```sh
make bench-vm
```
