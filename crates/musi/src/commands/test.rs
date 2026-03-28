use std::cell::RefCell;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use std::rc::Rc;

use clap::Args;
use musi::driver::{compile_project, emit_project_diagnostics};
use musi_vm::{ArrayValue, RuntimeHost, Value, ValueView, Vm, VmError};
use music_emit::{emit_project, write_seam};

#[derive(Args)]
pub struct TestArgs {
    /// Optional file or directory to test. Defaults to the current directory.
    pub path: Option<PathBuf>,
    /// Substring filter over joined suite/test names.
    #[arg(long)]
    pub grep: Option<String>,
}

#[derive(Clone)]
struct TestSuite {
    name: String,
    children: Vec<TestNode>,
}

#[derive(Clone)]
enum TestNode {
    Suite(TestSuite),
    Case(TestCase),
    Hook(TestHook),
}

#[derive(Clone)]
struct TestCase {
    name: String,
    body: Value,
}

#[derive(Clone, Copy)]
enum HookKind {
    BeforeAll,
    AfterAll,
    BeforeEach,
    AfterEach,
}

#[derive(Clone)]
struct TestHook {
    kind: HookKind,
    body: Value,
}

struct TestStats {
    passed: usize,
    failed: usize,
}

impl TestStats {
    const fn new() -> Self {
        Self {
            passed: 0,
            failed: 0,
        }
    }
}

pub fn run(args: &TestArgs) -> ExitCode {
    let target = args.path.clone().unwrap_or_else(|| PathBuf::from("."));
    let files = match collect_test_files(&target) {
        Ok(files) => files,
        Err(e) => {
            eprintln!("error: {e}");
            return ExitCode::from(2);
        }
    };

    if files.is_empty() {
        eprintln!("error: no *.test.ms files found under {}", target.display());
        return ExitCode::FAILURE;
    }

    let mut total = TestStats::new();
    for file in files {
        match run_test_file(&file, args.grep.as_deref()) {
            Ok(stats) => {
                total.passed += stats.passed;
                total.failed += stats.failed;
            }
            Err(message) => {
                eprintln!("error: {}: {message}", file.display());
                total.failed += 1;
            }
        }
    }

    if total.failed == 0 {
        println!("\nPASS  {} passed", total.passed);
        ExitCode::SUCCESS
    } else {
        println!("\nFAIL  {} passed, {} failed", total.passed, total.failed);
        ExitCode::FAILURE
    }
}

fn run_test_file(path: &Path, grep: Option<&str>) -> Result<TestStats, String> {
    if !is_test_file(path) {
        return Err("expected a *.test.ms file".into());
    }

    let analysis = compile_project(path).map_err(|e| e.to_string())?;
    emit_project_diagnostics(&analysis);
    if analysis.has_errors {
        return Err("compilation failed".into());
    }

    let canonical = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    let _module_id = analysis
        .project
        .graph
        .lookup(&canonical)
        .ok_or_else(|| "entry test module missing from project graph".to_owned())?;

    let emitted = emit_project(analysis.project).map_err(|e| e.to_string())?;
    let bytes = write_seam(&emitted.module);
    let program = musi_vm::load(&bytes).map_err(|e| format!("load failed; {e}"))?;

    let effect_id = program
        .effect_id("musi:test", "Test")
        .ok_or_else(|| "missing emitted `musi:test::Test` effect metadata".to_owned())?;
    let emit_op_id = program
        .effect_op_id(effect_id, "emit")
        .ok_or_else(|| "missing emitted `musi:test::Test.emit` operation metadata".to_owned())?;
    let collector = Rc::new(RefCell::new(TestCollector::new(default_suite_name(path))));
    let mut vm = Vm::with_host(program, Box::new(TestCollectorHandler {
        collector: Rc::clone(&collector),
        effect_id,
        emit_op_id,
    }));
    vm.initialize().map_err(|e| e.to_string())?;
    if let Err(error) = vm.invoke_export("test", &[]) {
        let collector = collector.borrow();
        return Err(collector
            .error
            .clone()
            .unwrap_or_else(|| format!("failed to invoke exported `test`; {error}")));
    }
    let suite = collector.borrow_mut().finish()?;

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

fn run_suite(
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

fn default_suite_name(path: &Path) -> String {
    path.file_stem()
        .and_then(|name| name.to_str())
        .map(str::to_owned)
        .unwrap_or_else(|| "tests".to_owned())
}

struct TestCollector {
    stack: Vec<TestSuite>,
    error: Option<String>,
}

impl TestCollector {
    fn new(root_name: String) -> Self {
        Self {
            stack: vec![TestSuite {
                name: root_name,
                children: Vec::new(),
            }],
            error: None,
        }
    }

    fn record_event(&mut self, vm: &Vm, value: Value) -> Result<(), String> {
        match parse_event(vm, value)? {
            TestEvent::SuiteStart(name) => {
                self.stack.push(TestSuite {
                    name,
                    children: Vec::new(),
                });
            }
            TestEvent::SuiteEnd => {
                if self.stack.len() <= 1 {
                    return Err("encountered suite end without an open suite".into());
                }
                let suite = self
                    .stack
                    .pop()
                    .ok_or_else(|| "missing suite to close".to_owned())?;
                self.current_suite()?.children.push(TestNode::Suite(suite));
            }
            TestEvent::Case(name, body) => {
                self.current_suite()?
                    .children
                    .push(TestNode::Case(TestCase { name, body }));
            }
            TestEvent::Hook(kind, body) => {
                self.current_suite()?
                    .children
                    .push(TestNode::Hook(TestHook { kind, body }));
            }
        }
        Ok(())
    }

    fn finish(&mut self) -> Result<TestSuite, String> {
        if self.stack.len() != 1 {
            return Err("one or more suites were not closed".into());
        }
        let root = self
            .stack
            .pop()
            .ok_or_else(|| "missing collected root suite".to_owned())?;
        if root.children.len() == 1 {
            if let Some(TestNode::Suite(suite)) = root.children.first() {
                return Ok(suite.clone());
            }
        }
        Ok(root)
    }

    fn current_suite(&mut self) -> Result<&mut TestSuite, String> {
        self.stack
            .last_mut()
            .ok_or_else(|| "missing active suite".to_owned())
    }
}

struct TestCollectorHandler {
    collector: Rc<RefCell<TestCollector>>,
    effect_id: u16,
    emit_op_id: u16,
}

impl RuntimeHost for TestCollectorHandler {
    fn handle_effect(
        &mut self,
        vm: &Vm,
        effect_id: u16,
        op_id: u16,
        payload: Value,
    ) -> Result<Option<Value>, VmError> {
        if effect_id != self.effect_id || op_id != self.emit_op_id {
            return Ok(None);
        }

        let mut collector = self.collector.borrow_mut();
        if let Err(error) = collector.record_event(vm, payload) {
            collector.error = Some(error);
            return Err(VmError::ExplicitPanic);
        }

        Ok(Some(Value::UNIT))
    }
}

enum TestEvent {
    SuiteStart(String),
    SuiteEnd,
    Case(String, Value),
    Hook(HookKind, Value),
}

fn parse_event(vm: &Vm, value: Value) -> Result<TestEvent, String> {
    let ArrayValue { elements, .. } = decode_array(vm, value)
        .ok_or_else(|| "test event is not an array".to_owned())?;
    if elements.is_empty() {
        return Err("test event must have at least a kind field".into());
    }
    let kind = decode_string(vm, elements[0])
        .ok_or_else(|| "test event kind must be a string".to_owned())?;

    match kind.as_str() {
        "suite_start" => {
            if elements.len() != 2 {
                return Err("suite_start event must have 2 fields".into());
            }
            let name = decode_string(vm, elements[1])
                .ok_or_else(|| "suite_start name must be a string".to_owned())?;
            Ok(TestEvent::SuiteStart(name))
        }
        "suite_end" => {
            if elements.len() != 1 {
                return Err("suite_end event must have 1 field".into());
            }
            Ok(TestEvent::SuiteEnd)
        }
        "case" => {
            if elements.len() != 3 {
                return Err("case event must have 3 fields".into());
            }
            let name = decode_string(vm, elements[1])
                .ok_or_else(|| "case name must be a string".to_owned())?;
            Ok(TestEvent::Case(name, elements[2]))
        }
        "before_all" | "after_all" | "before_each" | "after_each" => {
            if elements.len() != 2 {
                return Err(format!("{kind} event must have 2 fields"));
            }
            let hook_kind = match kind.as_str() {
                "before_all" => HookKind::BeforeAll,
                "after_all" => HookKind::AfterAll,
                "before_each" => HookKind::BeforeEach,
                "after_each" => HookKind::AfterEach,
                _ => unreachable!(),
            };
            Ok(TestEvent::Hook(hook_kind, elements[1]))
        }
        other => Err(format!("unknown test event kind `{other}`")),
    }
}

fn interpret_outcome(vm: &Vm, value: Value) -> Result<Outcome, String> {
    let ArrayValue { elements, .. } = decode_array(vm, value)
        .ok_or_else(|| "test result must be an outcome record".to_owned())?;
    if elements.len() != 2 {
        return Err("outcome record must have 2 fields".into());
    }

    let (passed, message) = if let Some(passed) = decode_bool(vm, elements[0]) {
        let message = decode_string(vm, elements[1])
            .ok_or_else(|| "outcome `message` field must be String".to_owned())?;
        (passed, message)
    } else if let Some(passed) = decode_bool(vm, elements[1]) {
        let message = decode_string(vm, elements[0])
            .ok_or_else(|| "outcome `message` field must be String".to_owned())?;
        (passed, message)
    } else {
        return Err("outcome `passed` field must be Bool".to_owned());
    };
    if passed {
        Ok(Outcome::Pass)
    } else {
        Ok(Outcome::Fail(message))
    }
}

fn decode_bool(vm: &Vm, value: Value) -> Option<bool> {
    if value.is_bool() {
        return Some(value.as_bool());
    }

    let ArrayValue { tag, elements: fields } = decode_array(vm, value)?;
    if !fields.is_empty() {
        return None;
    }

    match decode_string(vm, tag)?.as_str() {
        "True" => Some(true),
        "False" => Some(false),
        _ => None,
    }
}

fn decode_array(vm: &Vm, value: Value) -> Option<ArrayValue> {
    match vm.inspect(value)? {
        ValueView::Array(array) => Some(array),
        _ => None,
    }
}

fn decode_string(vm: &Vm, value: Value) -> Option<String> {
    match vm.inspect(value)? {
        ValueView::String(s) => Some(s),
        _ => None,
    }
}

enum Outcome {
    Pass,
    Fail(String),
}

fn indent(depth: usize) -> String {
    "  ".repeat(depth)
}

fn collect_test_files(target: &Path) -> Result<Vec<PathBuf>, std::io::Error> {
    if target.is_file() {
        return Ok(is_test_file(target)
            .then_some(target.to_path_buf())
            .into_iter()
            .collect());
    }

    let mut files = Vec::new();
    collect_test_files_recursive(target, &mut files)?;
    files.sort();
    Ok(files)
}

fn is_test_file(path: &Path) -> bool {
    path.file_name()
        .and_then(|name| name.to_str())
        .is_some_and(|name| name.ends_with(".test.ms"))
}

fn collect_test_files_recursive(dir: &Path, out: &mut Vec<PathBuf>) -> Result<(), std::io::Error> {
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            collect_test_files_recursive(&path, out)?;
            continue;
        }
        if is_test_file(&path) {
            out.push(path);
        }
    }
    Ok(())
}
