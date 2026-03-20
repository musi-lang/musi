//! Test runner - vitest-style describe/it with structured output.

import "@std/rt" as rt;

let mut suites := [];
let mut current_tests := [];
let mut current_failure : ?String := .None;

/// Registers a named test suite; test cases inside suite_fn are collected via `it`.
export let describe := (name, suite_fn) => (
  let tests := [];
  let prev := current_tests;
  current_tests <- tests;
  suite_fn();
  current_tests <- prev;
  rt.arr_push(suites, .{ name := name, tests := tests })
);

/// Registers a single named test case to run within the current describe block.
export let it := (name, test_fn) => (
  rt.arr_push(current_tests, .{ name := name, body := test_fn })
);

/// Records a test failure with the given message; only the first failure per test is kept.
export let fail := (msg : String) => (
  (current_failure <- .Some(msg) if current_failure = .None | () if _)
);

/// Runs all registered suites, prints results, and returns the failure count.
export let run := () => (
  let filter := rt.__test_filter();
  let has_filter := rt.str_len(filter) > 0;
  let mut total := 0;
  let mut passed := 0;
  let mut failed := 0;
  let mut skipped := 0;
  let mut fail_details := [];

  let run_suite := (suite) => (
    let test_count := rt.arr_len(suite.tests);
    let mut suite_passed := 0;
    let mut suite_failed := 0;
    let mut suite_skipped := 0;
    let mut results := [];

    let step := (i) => (
      (
        let entry := suite.tests.[i];
        (
          (
            skipped <- skipped + 1;
            suite_skipped <- suite_skipped + 1
          ) if has_filter and not rt.str_contains(entry.name, filter)
          | (
            total <- total + 1;
            current_failure <- .None;
            entry.body();
            match current_failure (
              .None => (
                passed <- passed + 1;
                suite_passed <- suite_passed + 1;
                rt.arr_push(results, .{ name := entry.name, passed := .True, msg := "" })
              )
              | .Some(msg) => (
                failed <- failed + 1;
                suite_failed <- suite_failed + 1;
                rt.arr_push(results, .{ name := entry.name, passed := .False, msg := msg });
                rt.arr_push(fail_details, .{ suite := suite.name, test := entry.name, msg := msg })
              )
            )
          ) if _
        );
        step(i + 1)
      ) if i < test_count
      | () if _
    );
    step(0);

    let has_results := rt.arr_len(results) > 0;
    (
      let marker := ("✓" if suite_failed = 0 | "✗" if _);
      rt.writeln(" " + marker + " " + suite.name);

      let result_count := rt.arr_len(results);
      let print_step := (i) => (
        (
          let r := results.[i];
          (
            rt.writeln("   ✓ " + r.name) if r.passed
            | (
              rt.writeln("   ✗ " + r.name);
              rt.writeln("     " + r.msg)
            ) if _
          );
          print_step(i + 1)
        ) if i < result_count
        | () if _
      );
      print_step(0)
    ) if has_results
    | () if _
  );

  let suite_count := rt.arr_len(suites);
  let suite_step := (i) => (
    (
      run_suite(suites.[i]);
      suite_step(i + 1)
    ) if i < suite_count
    | () if _
  );
  suite_step(0);

  rt.writeln("");
  rt.writeln(
    " Tests  " + rt.show(passed) + " passed"
    + (" | " + rt.show(failed) + " failed" if failed > 0 | "" if _)
    + (" | " + rt.show(skipped) + " skipped" if skipped > 0 | "" if _)
  );

  suites <- [];

  failed
);
