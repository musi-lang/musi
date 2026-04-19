# Musi VM C# Comparison Baselines

This benchmark harness gives the Rust `musi_vm` Criterion numbers context.

It deliberately reports two families:

- `compiled/*`: direct C#/.NET code for the same workload. This is an aspirational lower bound, not a fair interpreter comparison.
- `interpreter/*`: boxed values, call frames, data tags, closures, and continuation-shaped effect handling. This is closer to the current Musi VM architecture, but still not identical.

Run:

```sh
dotnet run -c Release --project benchmarks/csharp/MusiComparisonBenchmarks -- --smoke
dotnet run -c Release --project benchmarks/csharp/MusiComparisonBenchmarks
```

Compare labels with `crates/musi_vm/benches/bench_vm.rs`.

The harness uses `Stopwatch` and best-of-rounds `ns/op` output instead of BenchmarkDotNet so it has no NuGet dependency and can run in repo-local smoke checks.
