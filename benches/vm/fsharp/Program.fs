open System
open System.Diagnostics
open System.Globalization
open System.Runtime.CompilerServices

let defaultRounds = 5
let defaultIterations = 100_000
let smokeRounds = 2
let smokeIterations = 10_000

CultureInfo.CurrentCulture <- CultureInfo.InvariantCulture
CultureInfo.CurrentUICulture <- CultureInfo.InvariantCulture

let isSmoke = Environment.GetCommandLineArgs() |> Array.exists (fun arg -> arg.Equals("--smoke", StringComparison.OrdinalIgnoreCase))
let rounds = if isSmoke then smokeRounds else defaultRounds
let iterations = if isSmoke then smokeIterations else defaultIterations

module Sink =
    let mutable value = 0L

[<Struct>]
type Measurement =
    { NanosecondsPerOperation: double }

type Workload =
    { Name: string
      Run: unit -> int64 }

module BenchmarkRunner =
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let consume value =
        Sink.value <- Sink.value ^^^ value

    let measure workload rounds iterations =
        for _ in 0 .. (iterations / 10) - 1 do
            consume (workload.Run())

        let mutable bestTicks = Int64.MaxValue
        for _ in 0 .. rounds - 1 do
            let started = Stopwatch.GetTimestamp()
            for _ in 0 .. iterations - 1 do
                consume (workload.Run())
            bestTicks <- min bestTicks (Stopwatch.GetTimestamp() - started)

        let seconds = double bestTicks / double Stopwatch.Frequency
        { NanosecondsPerOperation = seconds * 1_000_000_000.0 / double iterations }

type MaybeInt =
    | SomeValue of int64
    | NoValue

type SmallModule() =
    let baseValue = 41L
    let offset = 1L

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    member _.Answer() = baseValue + offset

module VmBaselines =
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let initSmallModule () =
        let moduleValue = SmallModule()
        moduleValue.Answer()

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let rec sum n acc =
        if n = 0L then acc else sum (n - 1L) (acc + n)

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let scalarRecursiveSum () =
        sum 200L 0L

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let makeAdder baseValue =
        fun value -> value + baseValue

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let apply f value =
        f value

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let closureCapture () =
        apply (makeAdder 41L) 1L

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let sequenceIndexMutation () =
        let grid = [| [| 1L; 2L |]; [| 3L; 4L |] |]
        grid[0][1] <- 42L
        grid[1][0] <- grid[0][1] + 1L
        grid[0][1] + grid[1][0]

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let dataMatchOption () =
        let selected = SomeValue 41L
        match selected with
        | SomeValue value -> value + 1L
        | NoValue -> 0L

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let handleReadLine valueClause readLine =
        let resumed = readLine id
        valueClause resumed

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let effectResumeEquivalent () =
        handleReadLine (fun value -> value + 1L) (fun _ -> 41L)

let workloads =
    [| { Name = "init_small_module"; Run = VmBaselines.initSmallModule }
       { Name = "scalar_recursive_sum"; Run = VmBaselines.scalarRecursiveSum }
       { Name = "closure_capture"; Run = VmBaselines.closureCapture }
       { Name = "sequence_index_mutation"; Run = VmBaselines.sequenceIndexMutation }
       { Name = "data_match_option"; Run = VmBaselines.dataMatchOption }
       { Name = "effect_resume_equivalent"; Run = VmBaselines.effectResumeEquivalent } |]

let mode = if isSmoke then "smoke" else "full"
printfn $"F# CLR VM baselines ({mode})"
printfn $"Runtime: {Environment.Version}"
printfn $"Iterations: {iterations}"
printfn ""

for workload in workloads do
    let measurement = BenchmarkRunner.measure workload rounds iterations
    printfn "fsharp/%-36s %12.1f ns/op" workload.Name measurement.NanosecondsPerOperation
