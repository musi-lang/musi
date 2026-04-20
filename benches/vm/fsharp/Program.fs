open System
open System.Diagnostics
open System.Globalization
open System.Runtime.CompilerServices

let defaultRounds = 5
let defaultIterations = 100_000
let defaultWarmupIterations = 10_000
let smokeRounds = 2
let smokeIterations = 10_000
let smokeWarmupIterations = 1_000

CultureInfo.CurrentCulture <- CultureInfo.InvariantCulture
CultureInfo.CurrentUICulture <- CultureInfo.InvariantCulture

module Sink =
    let mutable value = 0L

[<Struct>]
type Measurement =
    { NanosecondsPerOperation: double }

type Workload =
    { Name: string
      Run: unit -> int64 }

type Phase =
    | Hot
    | Cold
    | Both
    member this.Label =
        match this with
        | Hot -> "hot"
        | Cold -> "cold"
        | Both -> "both"
    member this.IncludesHot =
        match this with
        | Hot
        | Both -> true
        | Cold -> false
    member this.IncludesCold =
        match this with
        | Cold
        | Both -> true
        | Hot -> false

type Profile =
    | Native
    | VmMode
    member this.Label =
        match this with
        | Native -> "native"
        | VmMode -> "vm_mode"

type RunSettings =
    { IsSmoke: bool
      Rounds: int
      Iterations: int
      WarmupIterations: int
      Phase: Phase
      Profile: Profile }

module ArgumentParsing =
    let private parsePositiveInt (flag: string) (value: string) =
        match Int32.TryParse value with
        | true, parsed when parsed > 0 -> parsed
        | _ -> invalidArg flag $"{flag} expects positive integer"

    let private parseNonNegativeInt (flag: string) (value: string) =
        match Int32.TryParse value with
        | true, parsed when parsed >= 0 -> parsed
        | _ -> invalidArg flag $"{flag} expects non-negative integer"

    let private parsePhase (raw: string) =
        match raw.ToLowerInvariant() with
        | "hot" -> Hot
        | "cold" -> Cold
        | "both" -> Both
        | _ -> invalidArg "--phase" $"unknown phase: {raw}"

    let private parseProfile (raw: string) =
        match raw.ToLowerInvariant() with
        | "native" -> Native
        | "vm_mode" -> VmMode
        | _ -> invalidArg "--profile" $"unknown profile: {raw}"

    let parse (args: string[]) =
        let mutable isSmoke = false
        let mutable rounds = defaultRounds
        let mutable iterations = defaultIterations
        let mutable warmupIterations = defaultWarmupIterations
        let mutable phase = Both
        let mutable profile = Native
        let mutable index = 0

        let requireValue flag =
            if index + 1 >= args.Length then
                invalidArg flag $"missing value for {flag}"
            args[index + 1]

        while index < args.Length do
            match args[index] with
            | "--smoke" ->
                isSmoke <- true
                index <- index + 1
            | "--rounds" ->
                rounds <- parsePositiveInt "--rounds" (requireValue "--rounds")
                index <- index + 2
            | "--iterations" ->
                iterations <- parsePositiveInt "--iterations" (requireValue "--iterations")
                index <- index + 2
            | "--warmup-iterations" ->
                warmupIterations <- parseNonNegativeInt "--warmup-iterations" (requireValue "--warmup-iterations")
                index <- index + 2
            | "--phase" ->
                phase <- parsePhase (requireValue "--phase")
                index <- index + 2
            | "--profile" ->
                profile <- parseProfile (requireValue "--profile")
                index <- index + 2
            | other -> invalidArg "args" $"unknown argument: {other}"

        if isSmoke then
            rounds <- smokeRounds
            iterations <- smokeIterations
            warmupIterations <- smokeWarmupIterations

        { IsSmoke = isSmoke
          Rounds = rounds
          Iterations = iterations
          WarmupIterations = warmupIterations
          Phase = phase
          Profile = profile }

module BenchmarkRunner =
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let consume value =
        Sink.value <- Sink.value ^^^ value

    let measureHot workload rounds iterations warmupIterations =
        for _ in 0 .. warmupIterations - 1 do
            consume (workload.Run())

        let mutable bestTicks = Int64.MaxValue
        for _ in 0 .. rounds - 1 do
            let started = Stopwatch.GetTimestamp()
            for _ in 0 .. iterations - 1 do
                consume (workload.Run())
            bestTicks <- min bestTicks (Stopwatch.GetTimestamp() - started)

        let seconds = double bestTicks / double Stopwatch.Frequency
        { NanosecondsPerOperation = seconds * 1_000_000_000.0 / double iterations }

    let measureCold workload iterations =
        let started = Stopwatch.GetTimestamp()
        for _ in 0 .. iterations - 1 do
            consume (workload.Run())
        let elapsed = Stopwatch.GetTimestamp() - started
        let seconds = double elapsed / double Stopwatch.Frequency
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
    let sharedGrid = [| [| 1L; 2L |]; [| 3L; 4L |] |]
    let sharedModule = SmallModule()

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let initSmallModule () =
        sharedModule.Answer()

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let constructSmallModule () =
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
        let grid = sharedGrid
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
       { Name = "effect_resume_equivalent"; Run = VmBaselines.effectResumeEquivalent }
       { Name = "construct_small_module"; Run = VmBaselines.constructSmallModule } |]

let settings = ArgumentParsing.parse (Environment.GetCommandLineArgs() |> Array.skip 1)
let mode = if settings.IsSmoke then "smoke" else "full"
printfn $"F# CLR VM baselines ({mode})"
printfn $"Runtime: {Environment.Version}"
printfn $"Config: profile={settings.Profile.Label} phase={settings.Phase.Label} rounds={settings.Rounds} iterations={settings.Iterations} warmup={settings.WarmupIterations}"
printfn ""

let printMeasurement phase workload measurement =
    printfn "fsharp/%s/%s/%-36s %12.1f ns/op" settings.Profile.Label phase workload measurement.NanosecondsPerOperation

for workload in workloads do
    if settings.Phase.IncludesCold then
        BenchmarkRunner.measureCold workload settings.Iterations
        |> printMeasurement "cold" workload.Name
    if settings.Phase.IncludesHot then
        BenchmarkRunner.measureHot workload settings.Rounds settings.Iterations settings.WarmupIterations
        |> printMeasurement "hot" workload.Name
