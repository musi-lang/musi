using System.Diagnostics;
using System.Globalization;
using System.Runtime.CompilerServices;

const int DefaultRounds = 5;
const int DefaultIterations = 100_000;
const int SmokeRounds = 2;
const int SmokeIterations = 10_000;

CultureInfo.CurrentCulture = CultureInfo.InvariantCulture;
CultureInfo.CurrentUICulture = CultureInfo.InvariantCulture;

var isSmoke = args.Any(arg => arg.Equals("--smoke", StringComparison.OrdinalIgnoreCase));
var rounds = isSmoke ? SmokeRounds : DefaultRounds;
var iterations = isSmoke ? SmokeIterations : DefaultIterations;

Console.WriteLine($"C# CLR VM baselines ({(isSmoke ? "smoke" : "full")})");
Console.WriteLine($"Runtime: {Environment.Version}");
Console.WriteLine($"Iterations: {iterations}");
Console.WriteLine();

foreach (var workload in Workloads.All)
{
    var measurement = BenchmarkRunner.Measure(workload, rounds, iterations);
    Console.WriteLine($"csharp/{workload.Name,-36} {measurement.NanosecondsPerOperation,12:N1} ns/op");
}

static class BenchmarkRunner
{
    public static Measurement Measure(Workload workload, int rounds, int iterations)
    {
        for (var index = 0; index < iterations / 10; index++)
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

    [MethodImpl(MethodImplOptions.NoInlining)]
    private static void Consume(long value)
    {
        Sink.Value ^= value;
    }
}

readonly record struct Measurement(double NanosecondsPerOperation);

sealed record Workload(string Name, Func<long> Run);

static class Sink
{
    public static long Value;
}

static class Workloads
{
    public static readonly Workload[] All =
    [
        new("init_small_module", VmBaselines.InitSmallModule),
        new("scalar_recursive_sum", VmBaselines.ScalarRecursiveSum),
        new("closure_capture", VmBaselines.ClosureCapture),
        new("sequence_index_mutation", VmBaselines.SequenceIndexMutation),
        new("data_match_option", VmBaselines.DataMatchOption),
        new("effect_resume_equivalent", VmBaselines.EffectResumeEquivalent),
    ];
}

static class VmBaselines
{
    [MethodImpl(MethodImplOptions.NoInlining)]
    public static long InitSmallModule()
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
        var grid = new[] { new[] { 1L, 2L }, new[] { 3L, 4L } };
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

readonly record struct MaybeInt(MaybeTag Tag, long Value)
{
    public static MaybeInt Some(long value)
    {
        return new MaybeInt(MaybeTag.Some, value);
    }
}

enum MaybeTag
{
    Some,
    None,
}
