import java.lang.management.ManagementFactory;
import java.util.Locale;
import java.util.function.LongSupplier;
import java.util.function.LongUnaryOperator;

public final class Program {
    private static final int DEFAULT_ROUNDS = 5;
    private static final int DEFAULT_ITERATIONS = 100_000;
    private static final int SMOKE_ROUNDS = 2;
    private static final int SMOKE_ITERATIONS = 10_000;

    public static void main(String[] args) {
        Locale.setDefault(Locale.ROOT);
        boolean isSmoke = false;
        for (String arg : args) {
            isSmoke |= "--smoke".equalsIgnoreCase(arg);
        }
        int rounds = isSmoke ? SMOKE_ROUNDS : DEFAULT_ROUNDS;
        int iterations = isSmoke ? SMOKE_ITERATIONS : DEFAULT_ITERATIONS;

        System.out.printf("Java 17 JVM baselines (%s)%n", isSmoke ? "smoke" : "full");
        System.out.printf("Runtime: %s%n", ManagementFactory.getRuntimeMXBean().getVmVersion());
        System.out.printf("Iterations: %d%n%n", iterations);

        for (Workload workload : Workloads.ALL) {
            Measurement measurement = BenchmarkRunner.measure(workload, rounds, iterations);
            System.out.printf(Locale.ROOT, "java/%-36s %12.1f ns/op%n", workload.name(), measurement.nanosecondsPerOperation());
        }
    }
}

final class BenchmarkRunner {
    private BenchmarkRunner() {}

    static Measurement measure(Workload workload, int rounds, int iterations) {
        for (int index = 0; index < iterations / 10; index++) {
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
    };

    private Workloads() {}
}

final class VmBaselines {
    private VmBaselines() {}

    static long initSmallModule() {
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
        long[][] grid = { { 1, 2 }, { 3, 4 } };
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
