import java.lang.management.ManagementFactory;
import java.util.Locale;
import java.util.function.LongSupplier;
import java.util.function.LongUnaryOperator;

public final class Program {
    static final int DEFAULT_ROUNDS = 5;
    static final int DEFAULT_ITERATIONS = 100_000;
    static final int DEFAULT_WARMUP_ITERATIONS = 10_000;
    static final int SMOKE_ROUNDS = 2;
    static final int SMOKE_ITERATIONS = 10_000;
    static final int SMOKE_WARMUP_ITERATIONS = 1_000;

    public static void main(String[] args) {
        Locale.setDefault(Locale.ROOT);
        Settings settings = Settings.parse(args);

        System.out.printf(
                "Java 17 JVM baselines (%s)%n",
                settings.isSmoke() ? "smoke" : "full");
        System.out.printf("Runtime: %s%n", ManagementFactory.getRuntimeMXBean().getVmVersion());
        System.out.printf(
                "Config: profile=%s phase=%s rounds=%d iterations=%d warmup=%d%n%n",
                settings.profile().label(),
                settings.phase().label(),
                settings.rounds(),
                settings.iterations(),
                settings.warmupIterations());

        for (Workload workload : Workloads.ALL) {
            if (settings.phase().includesCold()) {
                Measurement measurement = BenchmarkRunner.measureCold(workload, settings.iterations());
                printMeasurement(settings, "cold", workload.name(), measurement);
            }
            if (settings.phase().includesHot()) {
                Measurement measurement = BenchmarkRunner.measureHot(
                        workload,
                        settings.rounds(),
                        settings.iterations(),
                        settings.warmupIterations());
                printMeasurement(settings, "hot", workload.name(), measurement);
            }
        }
    }

    private static void printMeasurement(
            Settings settings,
            String phase,
            String workload,
            Measurement measurement) {
        System.out.printf(
                Locale.ROOT,
                "java/%s/%s/%-36s %12.1f ns/op%n",
                settings.profile().label(),
                phase,
                workload,
                measurement.nanosecondsPerOperation());
    }
}

final class BenchmarkRunner {
    private BenchmarkRunner() {}

    static Measurement measureHot(
            Workload workload,
            int rounds,
            int iterations,
            int warmupIterations) {
        for (int index = 0; index < warmupIterations; index++) {
            consume(workload.execute());
        }

        long bestNanos = Long.MAX_VALUE;
        for (int round = 0; round < rounds; round++) {
            long started = System.nanoTime();
            for (int index = 0; index < iterations; index++) {
                consume(workload.execute());
            }
            bestNanos = Math.min(bestNanos, System.nanoTime() - started);
        }
        return new Measurement((double) bestNanos / (double) iterations);
    }

    static Measurement measureCold(Workload workload, int iterations) {
        long started = System.nanoTime();
        for (int index = 0; index < iterations; index++) {
            consume(workload.execute());
        }
        long elapsed = System.nanoTime() - started;
        return new Measurement((double) elapsed / (double) iterations);
    }

    static void consume(long value) {
        Sink.value ^= value;
    }
}

record Measurement(double nanosecondsPerOperation) {}

record Workload(String name, LongSupplier run) {
    long execute() {
        return run.getAsLong();
    }
}

enum Phase {
    HOT("hot"),
    COLD("cold"),
    BOTH("both");

    private final String label;

    Phase(String label) {
        this.label = label;
    }

    String label() {
        return label;
    }

    boolean includesHot() {
        return this == HOT || this == BOTH;
    }

    boolean includesCold() {
        return this == COLD || this == BOTH;
    }

    static Phase parse(String raw) {
        return switch (raw.toLowerCase(Locale.ROOT)) {
            case "hot" -> HOT;
            case "cold" -> COLD;
            case "both" -> BOTH;
            default -> throw new IllegalArgumentException("unknown phase: " + raw);
        };
    }
}

enum Profile {
    NATIVE("native"),
    VM_MODE("vm_mode");

    private final String label;

    Profile(String label) {
        this.label = label;
    }

    String label() {
        return label;
    }

    static Profile parse(String raw) {
        return switch (raw.toLowerCase(Locale.ROOT)) {
            case "native" -> NATIVE;
            case "vm_mode" -> VM_MODE;
            default -> throw new IllegalArgumentException("unknown profile: " + raw);
        };
    }
}

record Settings(
        boolean isSmoke,
        int rounds,
        int iterations,
        int warmupIterations,
        Phase phase,
        Profile profile) {
    static Settings parse(String[] args) {
        boolean smoke = false;
        int rounds = Program.DEFAULT_ROUNDS;
        int iterations = Program.DEFAULT_ITERATIONS;
        int warmupIterations = Program.DEFAULT_WARMUP_ITERATIONS;
        Phase phase = Phase.BOTH;
        Profile profile = Profile.NATIVE;
        for (int index = 0; index < args.length; index++) {
            String arg = args[index];
            if ("--smoke".equalsIgnoreCase(arg)) {
                smoke = true;
                continue;
            }
            if ("--rounds".equalsIgnoreCase(arg)) {
                rounds = parsePositiveInt(requireValue(args, ++index, arg), arg);
                continue;
            }
            if ("--iterations".equalsIgnoreCase(arg)) {
                iterations = parsePositiveInt(requireValue(args, ++index, arg), arg);
                continue;
            }
            if ("--warmup-iterations".equalsIgnoreCase(arg)) {
                warmupIterations = parseNonNegativeInt(requireValue(args, ++index, arg), arg);
                continue;
            }
            if ("--phase".equalsIgnoreCase(arg)) {
                phase = Phase.parse(requireValue(args, ++index, arg));
                continue;
            }
            if ("--profile".equalsIgnoreCase(arg)) {
                profile = Profile.parse(requireValue(args, ++index, arg));
                continue;
            }
            throw new IllegalArgumentException("unknown argument: " + arg);
        }
        if (smoke) {
            rounds = Program.SMOKE_ROUNDS;
            iterations = Program.SMOKE_ITERATIONS;
            warmupIterations = Program.SMOKE_WARMUP_ITERATIONS;
        }
        return new Settings(smoke, rounds, iterations, warmupIterations, phase, profile);
    }

    private static String requireValue(String[] args, int index, String flag) {
        if (index >= args.length) {
            throw new IllegalArgumentException("missing value for " + flag);
        }
        return args[index];
    }

    private static int parsePositiveInt(String raw, String flag) {
        int parsed = Integer.parseInt(raw);
        if (parsed <= 0) {
            throw new IllegalArgumentException(flag + " expects positive integer");
        }
        return parsed;
    }

    private static int parseNonNegativeInt(String raw, String flag) {
        int parsed = Integer.parseInt(raw);
        if (parsed < 0) {
            throw new IllegalArgumentException(flag + " expects non-negative integer");
        }
        return parsed;
    }
}

final class Sink {
    static volatile long value;

    private Sink() {}
}

final class Workloads {
    static final Workload[] ALL = {
        new Workload("init_small_module", VmBaselines::initSmallModule),
        new Workload("scalar_recursive_sum", VmBaselines::scalarRecursiveSum),
        new Workload("closure_capture", VmBaselines::closureCapture),
        new Workload("sequence_index_mutation", VmBaselines::sequenceIndexMutation),
        new Workload("data_match_option", VmBaselines::dataMatchOption),
        new Workload("effect_resume_equivalent", VmBaselines::effectResumeEquivalent),
        new Workload("construct_small_module", VmBaselines::constructSmallModule),
    };

    private Workloads() {}
}

final class VmBaselines {
    private static final long[][] SHARED_GRID = {{1, 2}, {3, 4}};
    private static final SmallModule SHARED_MODULE = new SmallModule();

    private VmBaselines() {}

    static long initSmallModule() {
        return SHARED_MODULE.answer();
    }

    static long constructSmallModule() {
        SmallModule module = new SmallModule();
        return module.answer();
    }

    static long scalarRecursiveSum() {
        return sum(200, 0);
    }

    static long closureCapture() {
        return apply(makeAdder(41), 1);
    }

    static long sequenceIndexMutation() {
        long[][] grid = SHARED_GRID;
        grid[0][1] = 42;
        grid[1][0] = grid[0][1] + 1;
        return grid[0][1] + grid[1][0];
    }

    static long dataMatchOption() {
        MaybeInt selected = MaybeInt.some(41);
        return switch (selected.tag()) {
            case SOME -> selected.value() + 1;
            case NONE -> 0;
        };
    }

    static long effectResumeEquivalent() {
        return handleReadLine(value -> value + 1, ignored -> 41);
    }

    private static long sum(long n, long acc) {
        return n == 0 ? acc : sum(n - 1, acc + n);
    }

    private static LongUnaryOperator makeAdder(long baseValue) {
        return value -> value + baseValue;
    }

    private static long apply(LongUnaryOperator function, long value) {
        return function.applyAsLong(value);
    }

    private static long handleReadLine(LongUnaryOperator valueClause, ResumeHandler readLine) {
        long resumed = readLine.resume(value -> value);
        return valueClause.applyAsLong(resumed);
    }

    private static final class SmallModule {
        private final long baseValue = 41;
        private final long offset = 1;

        long answer() {
            return baseValue + offset;
        }
    }
}

@FunctionalInterface
interface ResumeHandler {
    long resume(LongUnaryOperator continuation);
}

record MaybeInt(MaybeTag tag, long value) {
    static MaybeInt some(long value) {
        return new MaybeInt(MaybeTag.SOME, value);
    }
}

enum MaybeTag {
    SOME,
    NONE,
}
