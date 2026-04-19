use std::mem::take;

use musi_foundation::test;
use musi_vm::{EffectCall, Value, VmError, VmErrorKind, VmHostContext, VmResult};

pub type TestSuitePath = Vec<Box<str>>;
pub type NativeTestCaseResultList = Vec<NativeTestCaseResult>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeTestCaseResult {
    pub suite: Box<str>,
    pub name: Box<str>,
    pub passed: bool,
}

impl NativeTestCaseResult {
    #[must_use]
    pub const fn new(suite: Box<str>, name: Box<str>, passed: bool) -> Self {
        Self {
            suite,
            name,
            passed,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeTestReport {
    pub module: Box<str>,
    pub cases: Box<[NativeTestCaseResult]>,
    pub stdout: Box<str>,
    pub stderr: Box<str>,
}

impl NativeTestReport {
    #[must_use]
    pub fn new(module: impl Into<Box<str>>, cases: Box<[NativeTestCaseResult]>) -> Self {
        Self {
            module: module.into(),
            cases,
            stdout: Box::default(),
            stderr: Box::default(),
        }
    }

    #[must_use]
    pub fn with_output(mut self, stdout: Box<str>, stderr: Box<str>) -> Self {
        self.stdout = stdout;
        self.stderr = stderr;
        self
    }
}

#[derive(Debug, Default)]
struct TestCollector {
    active_suites: TestSuitePath,
    cases: NativeTestCaseResultList,
}

#[derive(Debug, Default)]
pub struct TestHost {
    collector: Option<TestCollector>,
}

impl TestHost {
    pub fn begin_session(&mut self) {
        self.collector = Some(TestCollector::default());
    }

    #[must_use]
    pub fn finish_session(&mut self, module: &str) -> NativeTestReport {
        let mut collector = self.collector.take().unwrap_or_default();
        NativeTestReport::new(module, take(&mut collector.cases).into_boxed_slice())
    }

    #[must_use]
    pub fn handle_effect(
        &mut self,
        ctx: &VmHostContext<'_>,
        effect: &EffectCall,
        args: &[Value],
    ) -> Option<VmResult<Value>> {
        if effect.effect_name() != test::EFFECT {
            return None;
        }
        let Some(collector) = self.collector.as_mut() else {
            return Some(Err(reject_test_effect(effect, "test session not active")));
        };
        Some(handle_test_effect(ctx, collector, effect, args).map(|()| Value::Unit))
    }
}

fn handle_test_effect(
    ctx: &VmHostContext<'_>,
    collector: &mut TestCollector,
    effect: &EffectCall,
    args: &[Value],
) -> VmResult {
    match effect.op_name() {
        test::SUITE_START_OP => {
            let [name] = args else {
                return Err(invalid_test_effect(effect));
            };
            let Some(name) = ctx.string(name) else {
                return Err(invalid_test_effect(effect));
            };
            collector.active_suites.push(name.as_str().into());
        }
        test::SUITE_END_OP => {
            if !args.is_empty() {
                return Err(invalid_test_effect(effect));
            }
            let _ = collector.active_suites.pop();
        }
        test::TEST_CASE_OP => {
            let [name, passed] = args else {
                return Err(invalid_test_effect(effect));
            };
            let Some(name) = ctx.string(name) else {
                return Err(invalid_test_effect(effect));
            };
            let Some(passed) = ctx.bool_flag(passed) else {
                return Err(invalid_test_effect(effect));
            };
            collector.cases.push(NativeTestCaseResult::new(
                suite_name(&collector.active_suites),
                name.as_str().into(),
                passed,
            ));
        }
        _ => return Err(invalid_test_effect(effect)),
    }
    Ok(())
}

fn reject_test_effect(effect: &EffectCall, reason: impl Into<Box<str>>) -> VmError {
    VmError::new(VmErrorKind::EffectRejected {
        effect: effect.effect_name().into(),
        op: Some(effect.op_name().into()),
        reason: reason.into(),
    })
}

fn invalid_test_effect(effect: &EffectCall) -> VmError {
    reject_test_effect(effect, "invalid test event")
}

fn suite_name(path: &[Box<str>]) -> Box<str> {
    path.iter()
        .map(Box::as_ref)
        .collect::<Vec<_>>()
        .join(" / ")
        .into_boxed_str()
}
