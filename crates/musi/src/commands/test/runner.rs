use std::path::Path;

use musi_vm::{Value, Vm};

use super::decode::interpret_outcome;
use super::load::run_test_file as load_test_file;
use super::model::{HookKind, Outcome, TestCase, TestNode, TestStats, TestSuite};

pub(super) fn run_test_file(path: &Path, grep: Option<&str>) -> Result<TestStats, String> {
    let (mut vm, suite) = load_test_file(path)?;

    println!("\n{}", path.display());
    let mut stats = TestStats::new();
    run_suite(
        &mut vm,
        &suite,
        grep,
        &mut Vec::new(),
        &[],
        &[],
        0,
        &mut stats,
    );
    Ok(stats)
}

pub(super) fn run_suite(
    vm: &mut Vm,
    suite: &TestSuite,
    grep: Option<&str>,
    names: &mut Vec<String>,
    inherited_before_each: &[Value],
    inherited_after_each: &[Value],
    depth: usize,
    stats: &mut TestStats,
) {
    if !suite_has_matching_test(suite, grep, names) {
        return;
    }

    println!("{}{}", indent(depth), suite.name);
    names.push(suite.name.clone());

    let mut before_all = Vec::new();
    let mut after_all = Vec::new();
    let mut before_each = Vec::new();
    let mut after_each = Vec::new();
    let mut ordered_children = Vec::new();

    for child in &suite.children {
        match child {
            TestNode::Hook(hook) => match hook.kind {
                HookKind::BeforeAll => before_all.push(hook.body),
                HookKind::AfterAll => after_all.push(hook.body),
                HookKind::BeforeEach => before_each.push(hook.body),
                HookKind::AfterEach => after_each.push(hook.body),
            },
            TestNode::Suite(_) | TestNode::Case(_) => ordered_children.push(child.clone()),
        }
    }

    if let Err(message) = run_hooks(vm, &before_all) {
        println!("{}FAIL  [before_all] {message}", indent(depth + 1));
        stats.failed += 1;
        let _ = names.pop();
        return;
    }

    let mut next_before_each = inherited_before_each.to_vec();
    next_before_each.extend(before_each.iter().copied());

    let mut next_after_each = after_each.clone();
    next_after_each.extend_from_slice(inherited_after_each);

    for child in ordered_children {
        match child {
            TestNode::Suite(child_suite) => run_suite(
                vm,
                &child_suite,
                grep,
                names,
                &next_before_each,
                &next_after_each,
                depth + 1,
                stats,
            ),
            TestNode::Case(case) => {
                if !test_matches_filter(names, &case.name, grep) {
                    continue;
                }
                run_case(
                    vm,
                    &case,
                    names,
                    &next_before_each,
                    &next_after_each,
                    depth + 1,
                    stats,
                );
            }
            TestNode::Hook(_) => {}
        }
    }

    if let Err(message) = run_hooks(vm, &after_all) {
        println!("{}FAIL  [after_all] {message}", indent(depth + 1));
        stats.failed += 1;
    }

    let _ = names.pop();
}

fn run_case(
    vm: &mut Vm,
    case: &TestCase,
    names: &[String],
    before_each: &[Value],
    after_each: &[Value],
    depth: usize,
    stats: &mut TestStats,
) {
    let prefix = indent(depth);
    if let Err(message) = run_hooks(vm, before_each) {
        println!("{prefix}FAIL  {} ({message})", case.name);
        stats.failed += 1;
        return;
    }

    let result = vm.invoke(case.body, &[]);
    let outcome = match result {
        Ok(value) => interpret_outcome(vm, value).unwrap_or_else(|message| Outcome::Fail(message)),
        Err(error) => Outcome::Fail(error.to_string()),
    };

    let after_outcome = run_hooks(vm, after_each)
        .map(|_| Outcome::Pass)
        .unwrap_or_else(Outcome::Fail);

    let final_outcome = match (outcome, after_outcome) {
        (Outcome::Pass, other) => other,
        (fail @ Outcome::Fail(_), Outcome::Pass) => fail,
        (Outcome::Fail(test_error), Outcome::Fail(hook_error)) => {
            Outcome::Fail(format!("{test_error}; after_each: {hook_error}"))
        }
    };

    match final_outcome {
        Outcome::Pass => {
            println!("{prefix}PASS  {}", joined_name(names, &case.name));
            stats.passed += 1;
        }
        Outcome::Fail(message) => {
            println!(
                "{prefix}FAIL  {} ({message})",
                joined_name(names, &case.name)
            );
            stats.failed += 1;
        }
    }
}

fn run_hooks(vm: &mut Vm, hooks: &[Value]) -> Result<(), String> {
    for &hook in hooks {
        let result = vm.invoke(hook, &[]).map_err(|e| e.to_string())?;
        match interpret_outcome(vm, result)? {
            Outcome::Pass => {}
            Outcome::Fail(message) => return Err(message),
        }
    }
    Ok(())
}

fn suite_has_matching_test(suite: &TestSuite, grep: Option<&str>, names: &mut Vec<String>) -> bool {
    names.push(suite.name.clone());
    let matched = suite.children.iter().any(|child| match child {
        TestNode::Suite(child_suite) => suite_has_matching_test(child_suite, grep, names),
        TestNode::Case(case) => test_matches_filter(names, &case.name, grep),
        TestNode::Hook(_) => false,
    });
    let _ = names.pop();
    matched
}

fn test_matches_filter(names: &[String], case_name: &str, grep: Option<&str>) -> bool {
    grep.is_none_or(|pattern| joined_name(names, case_name).contains(pattern))
}

fn joined_name(names: &[String], case_name: &str) -> String {
    let mut full = names.join(" > ");
    if !full.is_empty() {
        full.push_str(" > ");
    }
    full.push_str(case_name);
    full
}

fn indent(depth: usize) -> String {
    "  ".repeat(depth)
}
