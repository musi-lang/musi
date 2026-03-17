//! Test runner — executes suites and reports results.

import "@std/rt" as rt;

export let TestResult := record {
  name   : String;
  passed : Bool;
  msg    : String;
};

export let SuiteResult := record {
  total  : Int;
  passed : Int;
  failed : Int;
  results : []TestResult;
};

export let run_test := (name, test_fn) => (
  let passed := test_fn();
  .{
    name   := name,
    passed := passed,
    msg    := ("PASS" if passed | "FAIL" if _),
  }
);

export let run_suite := (suite) => (
  let total := rt.arr_len(suite);
  let mut results := [];
  let mut passed_count := 0;
  let step := (i) => (
    (
      let pair := suite.[i];
      let r := run_test(pair.0, pair.1);
      rt.arr_push(results, r);
      (passed_count <- passed_count + 1 if r.passed | () if _);
      step(i + 1)
    ) if i < total
    | () if _
  );
  step(0);
  .{
    total   := total,
    passed  := passed_count,
    failed  := total - passed_count,
    results := results,
  }
);

export let report := (suite_result) => (
  let len := rt.arr_len(suite_result.results);
  let step := (i) => (
    (
      let r := suite_result.results.[i];
      rt.writeln("[" + r.msg + "] " + r.name);
      step(i + 1)
    ) if i < len
    | () if _
  );
  step(0);
  rt.writeln("─────────────────────────────");
  rt.writeln(
    rt.show(suite_result.passed) + "/" + rt.show(suite_result.total) + " passed"
    + (", " + rt.show(suite_result.failed) + " failed" if suite_result.failed > 0 | "" if _)
  );
  suite_result.failed
);
