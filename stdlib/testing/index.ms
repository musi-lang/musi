//! Vitest-style test framework.

export let runner := import "./runner";

let expect_mod := import "./expect";
export let expect := expect_mod.expect;
export let describe := runner.describe;
export let it := runner.it;
export let run := runner.run;
