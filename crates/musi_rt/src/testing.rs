use std::cell::RefCell;
use std::mem::take;
use std::rc::Rc;

use musi_native::NativeHost;
use musi_vm::{EffectCall, ForeignCall, Value, VmError, VmErrorKind, VmHost, VmResult};

pub type TestSuitePath = Vec<Box<str>>;
pub type RuntimeTestCaseResultList = Vec<RuntimeTestCaseResult>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeTestCaseResult {
    pub suite: Box<str>,
    pub name: Box<str>,
    pub passed: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeTestReport {
    pub module: Box<str>,
    pub cases: Box<[RuntimeTestCaseResult]>,
}

#[derive(Debug, Default)]
pub struct TestCollector {
    active_suites: TestSuitePath,
    cases: RuntimeTestCaseResultList,
}

pub struct TestRuntimeHost {
    native: NativeHost,
    collector: Rc<RefCell<TestCollector>>,
}

impl TestCollector {
    pub fn suite_start(&mut self, name: &str) {
        self.active_suites.push(name.into());
    }

    pub fn suite_end(&mut self) {
        let _ = self.active_suites.pop();
    }

    pub fn push_case(&mut self, name: &str, passed: bool) {
        self.cases.push(RuntimeTestCaseResult {
            suite: suite_name(&self.active_suites),
            name: name.into(),
            passed,
        });
    }

    pub fn finish_report(&mut self, module: &str) -> RuntimeTestReport {
        RuntimeTestReport {
            module: module.into(),
            cases: take(&mut self.cases).into_boxed_slice(),
        }
    }
}

impl TestRuntimeHost {
    pub const fn new(native: NativeHost, collector: Rc<RefCell<TestCollector>>) -> Self {
        Self { native, collector }
    }
}

impl VmHost for TestRuntimeHost {
    fn call_foreign(&mut self, foreign: &ForeignCall, args: &[Value]) -> VmResult<Value> {
        self.native.call_foreign(foreign, args)
    }

    fn handle_effect(&mut self, effect: &EffectCall, args: &[Value]) -> VmResult<Value> {
        if effect.effect_name() == "musi:test::Test" {
            handle_test_effect(&self.collector, effect, args)?;
            return Ok(Value::Unit);
        }
        self.native.handle_effect(effect, args)
    }
}

pub fn handle_test_effect(
    collector: &Rc<RefCell<TestCollector>>,
    effect: &EffectCall,
    args: &[Value],
) -> VmResult {
    let mut collector = collector.borrow_mut();
    match effect.op_name() {
        "suiteStart" => {
            let [Value::String(name)] = args else {
                return Err(invalid_test_effect(effect));
            };
            collector.suite_start(name.as_ref());
        }
        "suiteEnd" => {
            if !args.is_empty() {
                return Err(invalid_test_effect(effect));
            }
            collector.suite_end();
        }
        "testCase" => {
            let [Value::String(name), Value::Bool(passed)] = args else {
                return Err(invalid_test_effect(effect));
            };
            collector.push_case(name.as_ref(), *passed);
        }
        _ => return Err(invalid_test_effect(effect)),
    }
    Ok(())
}

fn invalid_test_effect(effect: &EffectCall) -> VmError {
    VmError::new(VmErrorKind::EffectRejected {
        effect: effect.effect_name().into(),
        op: Some(effect.op_name().into()),
        reason: "invalid test event".into(),
    })
}

fn suite_name(path: &[Box<str>]) -> Box<str> {
    path.iter()
        .map(Box::as_ref)
        .collect::<Vec<_>>()
        .join(" / ")
        .into_boxed_str()
}
