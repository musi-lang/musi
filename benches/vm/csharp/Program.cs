using System.Diagnostics;
using System.Globalization;
using System.Runtime.CompilerServices;

public static class Program
{
    private const int DefaultRounds = 5;
    private const int DefaultIterations = 100_000;
    private const int DefaultWarmupIterations = 10_000;
    private const int SmokeRounds = 2;
    private const int SmokeIterations = 10_000;
    private const int SmokeWarmupIterations = 1_000;

    public static void Main(string[] args)
    {
        CultureInfo.CurrentCulture = CultureInfo.InvariantCulture;
        CultureInfo.CurrentUICulture = CultureInfo.InvariantCulture;

        var settings = Settings.Parse(args);

        Console.WriteLine($"C# CLR VM baselines ({(settings.IsSmoke ? "smoke" : "full")})");
        Console.WriteLine($"Runtime: {Environment.Version}");
        Console.WriteLine(
            $"Config: profile={settings.Profile.Label} phase={settings.Phase.Label} workload={settings.WorkloadName} rounds={settings.Rounds} iterations={settings.Iterations} warmup={settings.WarmupIterations}");
        Console.WriteLine();

        foreach (var workload in Workloads.All)
        {
            if (!settings.Includes(workload.Name))
            {
                continue;
            }
            if (settings.Phase.IncludesCold)
            {
                var measurement = BenchmarkRunner.MeasureCold(workload, settings.Iterations);
                PrintMeasurement(settings, "cold", workload.Name, measurement);
            }
            if (settings.Phase.IncludesHot)
            {
                var measurement = BenchmarkRunner.MeasureHot(
                    workload,
                    settings.Rounds,
                    settings.Iterations,
                    settings.WarmupIterations);
                PrintMeasurement(settings, "hot", workload.Name, measurement);
            }
        }
    }

    private static void PrintMeasurement(Settings settings, string phase, string workload, Measurement measurement)
    {
        Console.WriteLine(
            $"csharp/{settings.Profile.Label}/{phase}/{workload,-36} {measurement.NanosecondsPerOperation,12:N1} ns/op");
    }

    private static class BenchmarkRunner
    {
        public static Measurement MeasureHot(Workload workload, int rounds, int iterations, int warmupIterations)
        {
            for (var index = 0; index < warmupIterations; index++)
            {
                Consume(workload.Run());
            }

            var bestTicks = long.MaxValue;
            for (var round = 0; round < rounds; round++)
            {
                var started = Stopwatch.GetTimestamp();
                for (var index = 0; index < iterations; index++)
                {
                    Consume(workload.Run());
                }
                bestTicks = Math.Min(bestTicks, Stopwatch.GetTimestamp() - started);
            }

            var seconds = (double)bestTicks / Stopwatch.Frequency;
            return new Measurement(seconds * 1_000_000_000.0 / iterations);
        }

        public static Measurement MeasureCold(Workload workload, int iterations)
        {
            var started = Stopwatch.GetTimestamp();
            for (var index = 0; index < iterations; index++)
            {
                Consume(workload.Run());
            }
            var elapsed = Stopwatch.GetTimestamp() - started;
            var seconds = (double)elapsed / Stopwatch.Frequency;
            return new Measurement(seconds * 1_000_000_000.0 / iterations);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        private static void Consume(long value)
        {
            Sink.Value ^= value;
        }
    }

    private readonly record struct Measurement(double NanosecondsPerOperation);
    private sealed record Workload(string Name, Func<long> Run);

    private readonly record struct Settings(
        bool IsSmoke,
        int Rounds,
        int Iterations,
        int WarmupIterations,
        Phase Phase,
        Profile Profile,
        string WorkloadName)
    {
        public static Settings Parse(string[] args)
        {
            var isSmoke = false;
            var rounds = DefaultRounds;
            var iterations = DefaultIterations;
            var warmupIterations = DefaultWarmupIterations;
            var phase = Phase.Both;
            var profile = Profile.Native;
            var workloadName = "all";
            for (var index = 0; index < args.Length; index++)
            {
                var arg = args[index];
                if (arg.Equals("--smoke", StringComparison.OrdinalIgnoreCase))
                {
                    isSmoke = true;
                    continue;
                }
                if (arg.Equals("--rounds", StringComparison.OrdinalIgnoreCase))
                {
                    rounds = ParsePositiveInt(RequireValue(args, ++index, arg), arg);
                    continue;
                }
                if (arg.Equals("--iterations", StringComparison.OrdinalIgnoreCase))
                {
                    iterations = ParsePositiveInt(RequireValue(args, ++index, arg), arg);
                    continue;
                }
                if (arg.Equals("--warmup-iterations", StringComparison.OrdinalIgnoreCase))
                {
                    warmupIterations = ParseNonNegativeInt(RequireValue(args, ++index, arg), arg);
                    continue;
                }
                if (arg.Equals("--phase", StringComparison.OrdinalIgnoreCase))
                {
                    phase = Phase.Parse(RequireValue(args, ++index, arg));
                    continue;
                }
                if (arg.Equals("--profile", StringComparison.OrdinalIgnoreCase))
                {
                    profile = Profile.Parse(RequireValue(args, ++index, arg));
                    continue;
                }
                if (arg.Equals("--workload", StringComparison.OrdinalIgnoreCase))
                {
                    workloadName = RequireValue(args, ++index, arg);
                    continue;
                }
                throw new ArgumentException($"unknown argument: {arg}");
            }

            if (isSmoke)
            {
                rounds = SmokeRounds;
                iterations = SmokeIterations;
                warmupIterations = SmokeWarmupIterations;
            }
            return new Settings(isSmoke, rounds, iterations, warmupIterations, phase, profile, workloadName);
        }

        public bool Includes(string candidate)
        {
            return WorkloadName.Equals("all", StringComparison.OrdinalIgnoreCase)
                || candidate.Equals(WorkloadName, StringComparison.OrdinalIgnoreCase);
        }

        private static string RequireValue(string[] args, int index, string flag)
        {
            if (index >= args.Length)
            {
                throw new ArgumentException($"missing value for {flag}");
            }
            return args[index];
        }

        private static int ParsePositiveInt(string raw, string flag)
        {
            if (!int.TryParse(raw, out var value) || value <= 0)
            {
                throw new ArgumentException($"{flag} expects positive integer");
            }
            return value;
        }

        private static int ParseNonNegativeInt(string raw, string flag)
        {
            if (!int.TryParse(raw, out var value) || value < 0)
            {
                throw new ArgumentException($"{flag} expects non-negative integer");
            }
            return value;
        }
    }

    private readonly record struct Phase(string Label, bool IncludesHot, bool IncludesCold)
    {
        public static readonly Phase Hot = new("hot", true, false);
        public static readonly Phase Cold = new("cold", false, true);
        public static readonly Phase Both = new("both", true, true);

        public static Phase Parse(string raw)
        {
            return raw.ToLowerInvariant() switch
            {
                "hot" => Hot,
                "cold" => Cold,
                "both" => Both,
                _ => throw new ArgumentException($"unknown phase: {raw}")
            };
        }
    }

    private readonly record struct Profile(string Label)
    {
        public static readonly Profile Native = new("native");
        public static readonly Profile VmMode = new("vm_mode");

        public static Profile Parse(string raw)
        {
            return raw.ToLowerInvariant() switch
            {
                "native" => Native,
                "vm_mode" => VmMode,
                _ => throw new ArgumentException($"unknown profile: {raw}")
            };
        }
    }

    private static class Sink
    {
        public static long Value;
        public static object? Object;
    }

    private static class Workloads
    {
        public static readonly Workload[] All =
        [
            new("init_small_module", VmBaselines.InitSmallModule),
            new("scalar_recursive_sum", VmBaselines.ScalarRecursiveSum),
            new("closure_capture", VmBaselines.ClosureCapture),
            new("sequence_index_mutation", VmBaselines.SequenceIndexMutation),
            new("data_match_option", VmBaselines.DataMatchOption),
            new("effect_resume_equivalent", VmBaselines.EffectResumeEquivalent),
            new("sequence_return_alloc", VmBaselines.SequenceReturnAlloc),
            new("sequence_return_forced_gc", VmBaselines.SequenceReturnForcedGc),
            new("construct_small_module", VmBaselines.ConstructSmallModule),
        ];
    }

    private static class VmBaselines
    {
        private static readonly long[][] SharedGrid =
        [
            [1, 2],
            [3, 4],
        ];

        private static readonly SmallModule SharedModule = new();

        [MethodImpl(MethodImplOptions.NoInlining)]
        public static long InitSmallModule()
        {
            return SharedModule.Answer();
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        public static long ConstructSmallModule()
        {
            var module = new SmallModule();
            return module.Answer();
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        public static long ScalarRecursiveSum()
        {
            return Sum(200, 0);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        public static long ClosureCapture()
        {
            return Apply(MakeAdder(41), 1);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        public static long SequenceIndexMutation()
        {
            var grid = SharedGrid;
            grid[0][1] = 42;
            grid[1][0] = grid[0][1] + 1;
            return grid[0][1] + grid[1][0];
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        public static long DataMatchOption()
        {
            var selected = MaybeInt.Some(41);
            return selected.Tag switch
            {
                MaybeTag.Some => selected.Value + 1,
                MaybeTag.None => 0,
                _ => throw new UnreachableException(),
            };
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        public static long EffectResumeEquivalent()
        {
            return HandleReadLine(value => value + 1, _ => 41);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        public static long SequenceReturnAlloc()
        {
            long[] values = [0, 1, 2, 3, 4, 5, 6, 7];
            Sink.Object = values;
            return values[7] + values.Length;
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        public static long SequenceReturnForcedGc()
        {
            var result = SequenceReturnAlloc();
            GC.Collect(GC.MaxGeneration, GCCollectionMode.Forced, blocking: true, compacting: false);
            return result;
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        private static long Sum(long n, long acc)
        {
            return n == 0 ? acc : Sum(n - 1, acc + n);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        private static Func<long, long> MakeAdder(long baseValue)
        {
            return value => value + baseValue;
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        private static long Apply(Func<long, long> function, long value)
        {
            return function(value);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        private static long HandleReadLine(Func<long, long> valueClause, Func<Func<long, long>, long> readLine)
        {
            var resumed = readLine(value => value);
            return valueClause(resumed);
        }

        private sealed class SmallModule
        {
            private readonly long baseValue = 41;
            private readonly long offset = 1;

            [MethodImpl(MethodImplOptions.NoInlining)]
            public long Answer()
            {
                return baseValue + offset;
            }
        }
    }

    private readonly record struct MaybeInt(MaybeTag Tag, long Value)
    {
        public static MaybeInt Some(long value)
        {
            return new MaybeInt(MaybeTag.Some, value);
        }
    }

    private enum MaybeTag
    {
        Some,
        None,
    }
}
