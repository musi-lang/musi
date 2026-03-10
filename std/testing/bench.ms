/// Run a benchmark, printing timing results.
export let benchmark : (() -> ()) ~> () under { IO };

/// Run a benchmark `n` times, printing aggregate timing results.
export let benchmark_n : (Int, () -> ()) ~> () under { IO };
