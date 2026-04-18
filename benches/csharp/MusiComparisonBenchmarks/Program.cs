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

Console.WriteLine($"Musi C# comparison baselines ({(isSmoke ? "smoke" : "full")})");
Console.WriteLine($"Runtime: {Environment.Version}");
Console.WriteLine($"Iterations: {iterations}");
Console.WriteLine();

foreach (var workload in Workloads.All)
{
    var measurement = BenchmarkRunner.Measure(workload, rounds, iterations);
    Console.WriteLine($"{workload.Name,-58} {measurement.NanosecondsPerOperation,12:N1} ns/op");
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
            var elapsed = Stopwatch.GetTimestamp() - started;
            bestTicks = Math.Min(bestTicks, elapsed);
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
        new("compiled/bench_vm_init_small_module", Compiled.InitSmallModule),
        new("compiled/bench_vm_call_scalar_recursive_sum", Compiled.ScalarRecursiveSum),
        new("compiled/bench_vm_closure_capture", Compiled.ClosureCapture),
        new("compiled/bench_vm_sequence_index_mutation", Compiled.SequenceIndexMutation),
        new("compiled/bench_vm_data_match_option", Compiled.DataMatchOption),
        new("compiled/bench_vm_effect_resume", Compiled.EffectResume),
        new("interpreter/bench_vm_init_small_module", Interpreter.InitSmallModule),
        new("interpreter/bench_vm_call_scalar_recursive_sum", Interpreter.ScalarRecursiveSum),
        new("interpreter/bench_vm_closure_capture", Interpreter.ClosureCapture),
        new("interpreter/bench_vm_sequence_index_mutation", Interpreter.SequenceIndexMutation),
        new("interpreter/bench_vm_data_match_option", Interpreter.DataMatchOption),
        new("interpreter/bench_vm_effect_resume", Interpreter.EffectResume),
    ];
}

static class Compiled
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
    public static long EffectResume()
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
        private readonly long _baseValue = 41;
        private readonly long _offset = 1;

        [MethodImpl(MethodImplOptions.NoInlining)]
        public long Answer()
        {
            return _baseValue + _offset;
        }
    }
}

static class Interpreter
{
    [MethodImpl(MethodImplOptions.NoInlining)]
    public static long InitSmallModule()
    {
        var module = new RuntimeModule();
        module.Globals["base"] = new IntValue(41);
        module.Globals["offset"] = new IntValue(1);
        return UnboxInt(module.Call("answer", []));
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static long ScalarRecursiveSum()
    {
        var module = new RuntimeModule();
        return UnboxInt(module.Call("sum", [new IntValue(200), new IntValue(0)]));
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static long ClosureCapture()
    {
        var module = new RuntimeModule();
        return UnboxInt(module.Call("closureCapture", [new IntValue(1)]));
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static long SequenceIndexMutation()
    {
        var module = new RuntimeModule();
        var grid = new SeqValue(
        [
            new SeqValue([new IntValue(1), new IntValue(2)]),
            new SeqValue([new IntValue(3), new IntValue(4)]),
        ]);
        return UnboxInt(module.Call("sequenceMutation", [grid]));
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static long DataMatchOption()
    {
        var module = new RuntimeModule();
        return UnboxInt(module.Call("dataMatchOption", [new IntValue(41)]));
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    public static long EffectResume()
    {
        var module = new RuntimeModule();
        return UnboxInt(module.Call("effectResume", []));
    }

    private static long UnboxInt(RuntimeValue value)
    {
        return ((IntValue)value).Value;
    }
}

sealed class RuntimeModule
{
    public Dictionary<string, RuntimeValue> Globals { get; } = [];

    [MethodImpl(MethodImplOptions.NoInlining)]
    public RuntimeValue Call(string name, RuntimeValue[] args)
    {
        Frames.Push(new CallFrame(name, args));
        try
        {
            return name switch
            {
                "answer" => new IntValue(((IntValue)Globals["base"]).Value + ((IntValue)Globals["offset"]).Value),
                "sum" => Sum((IntValue)args[0], (IntValue)args[1]),
                "closureCapture" => ClosureCapture((IntValue)args[0]),
                "apply" => ((ClosureValue)args[0]).Invoke(args[1]),
                "sequenceMutation" => SequenceMutation((SeqValue)args[0]),
                "dataMatchOption" => DataMatchOption((IntValue)args[0]),
                "effectResume" => EffectResume(),
                _ => throw new InvalidOperationException($"unknown method `{name}`"),
            };
        }
        finally
        {
            Frames.Pop();
        }
    }

    private Stack<CallFrame> Frames { get; } = new();

    private RuntimeValue Sum(IntValue n, IntValue acc)
    {
        return n.Value == 0
            ? acc
            : Call("sum", [new IntValue(n.Value - 1), new IntValue(acc.Value + n.Value)]);
    }

    private RuntimeValue ClosureCapture(IntValue value)
    {
        var closure = new ClosureValue(argument => new IntValue(((IntValue)argument).Value + 41));
        return Call("apply", [closure, value]);
    }

    private static RuntimeValue SequenceMutation(SeqValue grid)
    {
        var first = (SeqValue)grid.Items[0];
        var second = (SeqValue)grid.Items[1];
        first.Items[1] = new IntValue(42);
        second.Items[0] = new IntValue(((IntValue)first.Items[1]).Value + 1);
        return new IntValue(((IntValue)first.Items[1]).Value + ((IntValue)second.Items[0]).Value);
    }

    private static RuntimeValue DataMatchOption(IntValue value)
    {
        var selected = new DataValue(0, [value]);
        return selected.Tag switch
        {
            0 => new IntValue(((IntValue)selected.Fields[0]).Value + 1),
            1 => new IntValue(0),
            _ => throw new UnreachableException(),
        };
    }

    private static RuntimeValue EffectResume()
    {
        var continuation = new ContinuationValue(value => value);
        var resumed = continuation.Resume(new IntValue(41));
        return new IntValue(((IntValue)resumed).Value + 1);
    }
}

readonly record struct CallFrame(string Name, RuntimeValue[] Locals);

abstract record RuntimeValue;

sealed record IntValue(long Value) : RuntimeValue;

sealed record SeqValue(RuntimeValue[] Items) : RuntimeValue;

sealed record DataValue(long Tag, RuntimeValue[] Fields) : RuntimeValue;

sealed record ClosureValue(Func<RuntimeValue, RuntimeValue> Invoke) : RuntimeValue;

sealed record ContinuationValue(Func<RuntimeValue, RuntimeValue> Resume) : RuntimeValue;

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
