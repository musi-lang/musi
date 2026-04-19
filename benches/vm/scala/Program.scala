import java.lang.management.ManagementFactory
import java.util.Locale

private object Settings:
  val DefaultRounds = 5
  val DefaultIterations = 100_000
  val SmokeRounds = 2
  val SmokeIterations = 10_000

@main def run(args: String*): Unit =
  Locale.setDefault(Locale.ROOT)

  val isSmoke = args.exists(_.equalsIgnoreCase("--smoke"))
  val rounds = if isSmoke then Settings.SmokeRounds else Settings.DefaultRounds
  val iterations = if isSmoke then Settings.SmokeIterations else Settings.DefaultIterations

  println(s"Scala 3 JVM baselines (${if isSmoke then "smoke" else "full"})")
  println(s"Runtime: ${ManagementFactory.getRuntimeMXBean.getVmVersion}")
  println(s"Iterations: $iterations")
  println()

  Workloads.All.foreach { workload =>
    val measurement = BenchmarkRunner.measure(workload, rounds, iterations)
    println(f"scala/${workload.name}%-36s ${measurement.nanosecondsPerOperation}%12.1f ns/op")
  }

private object BenchmarkRunner:
  def measure(workload: Workload, rounds: Int, iterations: Int): Measurement =
    var index = 0
    while index < iterations / 10 do
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

  private def consume(value: Long): Unit =
    Sink.value = Sink.value ^ value

private final case class Measurement(nanosecondsPerOperation: Double)
private final case class Workload(name: String, run: () => Long)

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
  )

private object VmBaselines:
  def initSmallModule(): Long =
    val module = SmallModule()
    module.answer()

  def scalarRecursiveSum(): Long =
    sum(200L, 0L)

  def closureCapture(): Long =
    apply(makeAdder(41L), 1L)

  def sequenceIndexMutation(): Long =
    val grid = Array(Array(1L, 2L), Array(3L, 4L))
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
