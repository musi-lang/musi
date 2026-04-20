import java.lang.management.ManagementFactory
import java.util.Locale

private object Settings:
  val DefaultRounds = 5
  val DefaultIterations = 100_000
  val DefaultWarmupIterations = 10_000
  val SmokeRounds = 2
  val SmokeIterations = 10_000
  val SmokeWarmupIterations = 1_000

@main def run(args: String*): Unit =
  Locale.setDefault(Locale.ROOT)
  val settings = RunSettings.parse(args.toArray)

  println(s"Scala 3 JVM baselines (${if settings.isSmoke then "smoke" else "full"})")
  println(s"Runtime: ${ManagementFactory.getRuntimeMXBean.getVmVersion}")
  println(
    s"Config: profile=${settings.profile.label} phase=${settings.phase.label} rounds=${settings.rounds} iterations=${settings.iterations} warmup=${settings.warmupIterations}"
  )
  println()

  Workloads.All.foreach { workload =>
    if settings.phase.includesCold then
      val measurement = BenchmarkRunner.measureCold(workload, settings.iterations)
      printMeasurement(settings, "cold", workload.name, measurement)
    if settings.phase.includesHot then
      val measurement =
        BenchmarkRunner.measureHot(workload, settings.rounds, settings.iterations, settings.warmupIterations)
      printMeasurement(settings, "hot", workload.name, measurement)
  }

private def printMeasurement(settings: RunSettings, phase: String, workload: String, measurement: Measurement): Unit =
  println(
    f"scala/${settings.profile.label}/$phase/$workload%-36s ${measurement.nanosecondsPerOperation}%12.1f ns/op"
  )

private object BenchmarkRunner:
  def measureHot(workload: Workload, rounds: Int, iterations: Int, warmupIterations: Int): Measurement =
    var index = 0
    while index < warmupIterations do
      consume(workload.run())
      index += 1

    var bestNanos = Long.MaxValue
    var round = 0
    while round < rounds do
      val started = System.nanoTime()
      index = 0
      while index < iterations do
        consume(workload.run())
        index += 1
      bestNanos = math.min(bestNanos, System.nanoTime() - started)
      round += 1

    Measurement(bestNanos.toDouble / iterations.toDouble)

  def measureCold(workload: Workload, iterations: Int): Measurement =
    val started = System.nanoTime()
    var index = 0
    while index < iterations do
      consume(workload.run())
      index += 1
    val elapsed = System.nanoTime() - started
    Measurement(elapsed.toDouble / iterations.toDouble)

  private def consume(value: Long): Unit =
    Sink.value = Sink.value ^ value

private final case class Measurement(nanosecondsPerOperation: Double)
private final case class Workload(name: String, run: () => Long)

private final case class Phase(label: String, includesHot: Boolean, includesCold: Boolean)

private object Phase:
  val Hot = Phase("hot", includesHot = true, includesCold = false)
  val Cold = Phase("cold", includesHot = false, includesCold = true)
  val Both = Phase("both", includesHot = true, includesCold = true)

  def parse(raw: String): Phase =
    raw.toLowerCase(Locale.ROOT) match
      case "hot" => Hot
      case "cold" => Cold
      case "both" => Both
      case _ => throw IllegalArgumentException(s"unknown phase: $raw")

private final case class Profile(label: String)

private object Profile:
  val Native = Profile("native")
  val VmMode = Profile("vm_mode")

  def parse(raw: String): Profile =
    raw.toLowerCase(Locale.ROOT) match
      case "native" => Native
      case "vm_mode" => VmMode
      case _ => throw IllegalArgumentException(s"unknown profile: $raw")

private final case class RunSettings(
    isSmoke: Boolean,
    rounds: Int,
    iterations: Int,
    warmupIterations: Int,
    phase: Phase,
    profile: Profile
)

private object RunSettings:
  def parse(args: Array[String]): RunSettings =
    var smoke = false
    var rounds = Settings.DefaultRounds
    var iterations = Settings.DefaultIterations
    var warmupIterations = Settings.DefaultWarmupIterations
    var phase = Phase.Both
    var profile = Profile.Native

    var index = 0
    while index < args.length do
      args(index) match
        case "--smoke" =>
          smoke = true
          index += 1
        case "--rounds" =>
          rounds = parsePositiveInt(requireValue(args, index + 1, "--rounds"), "--rounds")
          index += 2
        case "--iterations" =>
          iterations = parsePositiveInt(requireValue(args, index + 1, "--iterations"), "--iterations")
          index += 2
        case "--warmup-iterations" =>
          warmupIterations = parseNonNegativeInt(
            requireValue(args, index + 1, "--warmup-iterations"),
            "--warmup-iterations"
          )
          index += 2
        case "--phase" =>
          phase = Phase.parse(requireValue(args, index + 1, "--phase"))
          index += 2
        case "--profile" =>
          profile = Profile.parse(requireValue(args, index + 1, "--profile"))
          index += 2
        case other =>
          throw IllegalArgumentException(s"unknown argument: $other")

    if smoke then
      rounds = Settings.SmokeRounds
      iterations = Settings.SmokeIterations
      warmupIterations = Settings.SmokeWarmupIterations

    RunSettings(smoke, rounds, iterations, warmupIterations, phase, profile)

  private def requireValue(args: Array[String], index: Int, flag: String): String =
    if index >= args.length then throw IllegalArgumentException(s"missing value for $flag")
    args(index)

  private def parsePositiveInt(raw: String, flag: String): Int =
    val parsed = raw.toIntOption.getOrElse(throw IllegalArgumentException(s"$flag expects positive integer"))
    if parsed <= 0 then throw IllegalArgumentException(s"$flag expects positive integer")
    parsed

  private def parseNonNegativeInt(raw: String, flag: String): Int =
    val parsed = raw.toIntOption.getOrElse(throw IllegalArgumentException(s"$flag expects non-negative integer"))
    if parsed < 0 then throw IllegalArgumentException(s"$flag expects non-negative integer")
    parsed

private object Sink:
  @volatile var value: Long = 0L

private object Workloads:
  val All: Array[Workload] = Array(
    Workload("init_small_module", VmBaselines.initSmallModule),
    Workload("scalar_recursive_sum", VmBaselines.scalarRecursiveSum),
    Workload("closure_capture", VmBaselines.closureCapture),
    Workload("sequence_index_mutation", VmBaselines.sequenceIndexMutation),
    Workload("data_match_option", VmBaselines.dataMatchOption),
    Workload("effect_resume_equivalent", VmBaselines.effectResumeEquivalent),
    Workload("construct_small_module", VmBaselines.constructSmallModule)
  )

private object VmBaselines:
  private val sharedGrid = Array(Array(1L, 2L), Array(3L, 4L))
  private val sharedModule = SmallModule()

  def initSmallModule(): Long =
    sharedModule.answer()

  def constructSmallModule(): Long =
    val module = SmallModule()
    module.answer()

  def scalarRecursiveSum(): Long =
    sum(200L, 0L)

  def closureCapture(): Long =
    apply(makeAdder(41L), 1L)

  def sequenceIndexMutation(): Long =
    val grid = sharedGrid
    grid(0)(1) = 42L
    grid(1)(0) = grid(0)(1) + 1L
    grid(0)(1) + grid(1)(0)

  def dataMatchOption(): Long =
    val selected: MaybeInt = MaybeInt.Some(41L)
    selected match
      case MaybeInt.Some(value) => value + 1L
      case MaybeInt.None => 0L

  def effectResumeEquivalent(): Long =
    handleReadLine(value => value + 1L, _ => 41L)

  private def sum(n: Long, acc: Long): Long =
    if n == 0L then acc else sum(n - 1L, acc + n)

  private def makeAdder(baseValue: Long): Long => Long =
    value => value + baseValue

  private def apply(function: Long => Long, value: Long): Long =
    function(value)

  private def handleReadLine(valueClause: Long => Long, readLine: (Long => Long) => Long): Long =
    val resumed = readLine(value => value)
    valueClause(resumed)

private final class SmallModule:
  private val baseValue = 41L
  private val offset = 1L

  def answer(): Long =
    baseValue + offset

private enum MaybeInt:
  case Some(value: Long)
  case None
