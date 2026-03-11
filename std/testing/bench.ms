/// Run a benchmark once and print timing results.
///
/// # Examples
///
/// ```musi
/// benchmark(() -> fib(30))
/// ```
export let benchmark : (() -> ()) ~> () under { IO };

/// Run a benchmark `n` times and print aggregate timing results.
///
/// # Examples
///
/// ```musi
/// benchmark_n(100, () -> fib(30))
/// ```
export let benchmark_n : (Int, () -> ()) ~> () under { IO };
