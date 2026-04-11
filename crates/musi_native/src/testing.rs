use std::mem::take;

use musi_foundation::test;
use musi_vm::{EffectCall, Value, VmError, VmErrorKind, VmResult};

pub type TestSuitePath = Vec<Box<str>>;
pub type NativeTestCaseResultList = Vec<NativeTestCaseResult>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeTestCaseResult {
    pub suite: Box<str>,
    pub name: Box<str>,
    pub passed: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeTestReport {
    pub module: Box<str>,
    pub cases: Box<[NativeTestCaseResult]>,
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
        NativeTestReport {
            module: module.into(),
            cases: take(&mut collector.cases).into_boxed_slice(),
        }
    }

    #[must_use]
    pub fn handle_effect(
        &mut self,
        effect: &EffectCall,
        args: &[Value],
    ) -> Option<VmResult<Value>> {
        if effect.effect_name() != test::EFFECT {
            return None;
        }
        let Some(collector) = self.collector.as_mut() else {
            return Some(Err(VmError::new(VmErrorKind::EffectRejected {
                effect: effect.effect_name().into(),
                op: Some(effect.op_name().into()),
                reason: "test session not active".into(),
            })));
        };
        Some(handle_test_effect(collector, effect, args).map(|()| Value::Unit))
    }
}

fn handle_test_effect(
    collector: &mut TestCollector,
    effect: &EffectCall,
    args: &[Value],
) -> VmResult {
    match effect.op_name() {
        test::SUITE_START_OP => {
            let [Value::String(name)] = args else {
                return Err(invalid_test_effect(effect));
            };
            collector.active_suites.push(name.as_ref().into());
        }
        test::SUITE_END_OP => {
            if !args.is_empty() {
                return Err(invalid_test_effect(effect));
            }
            let _ = collector.active_suites.pop();
        }
        test::TEST_CASE_OP => {
            let [Value::String(name), Value::Bool(passed)] = args else {
                return Err(invalid_test_effect(effect));
            };
            collector.cases.push(NativeTestCaseResult {
                suite: suite_name(&collector.active_suites),
                name: name.as_ref().into(),
                passed: *passed,
            });
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
