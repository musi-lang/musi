//! Vitest-style test framework.

/// Low-level test runner (describe, it, run, fail).
export let runner := import "./runner";

let expect_mod := import "./expect";

/// Creates a fluent matcher for the given actual value.
export let expect := expect_mod.expect;

/// Registers a named test suite containing one or more `it` tests.
export let describe := runner.describe;

/// Registers a single named test case inside a describe block.
export let it := runner.it;

/// Executes all registered test suites and prints results.
export let run := runner.run;
