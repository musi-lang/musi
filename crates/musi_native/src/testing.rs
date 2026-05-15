use std::mem::take;

use musi_foundation::test;
use musi_vm::{ForeignCall, Value, VmError, VmErrorKind, VmHostContext, VmResult};

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
    pub fn handle_foreign(
        &mut self,
        ctx: &VmHostContext<'_>,
        foreign: &ForeignCall,
        args: &[Value],
    ) -> Option<VmResult<Value>> {
        let op = foreign
            .name()
            .strip_prefix("musi:test::")
            .and_then(|name| name.strip_prefix("Musi__"))?;
        let Some(collector) = self.collector.as_mut() else {
            return Some(Err(VmError::new(VmErrorKind::ForeignCallRejected {
                foreign: foreign.name().into(),
            })));
        };
        Some(handle_test_foreign(ctx, collector, foreign, op, args).map(|()| Value::Unit))
    }
}

fn handle_test_foreign(
    ctx: &VmHostContext<'_>,
    collector: &mut TestCollector,
    foreign: &ForeignCall,
    op: &str,
    args: &[Value],
) -> VmResult {
    match op {
        test::SUITE_START_OP => {
            let [name] = args else {
                return Err(invalid_test_foreign(foreign));
            };
            let Some(name) = ctx.string(name) else {
                return Err(invalid_test_foreign(foreign));
            };
            collector.active_suites.push(name.as_str().into());
        }
        test::SUITE_END_OP => {
            if !args.is_empty() {
                return Err(invalid_test_foreign(foreign));
            }
            let _ = collector.active_suites.pop();
        }
        test::TEST_CASE_OP => {
            let [name, passed] = args else {
                return Err(invalid_test_foreign(foreign));
            };
            let Some(name) = ctx.string(name) else {
                return Err(invalid_test_foreign(foreign));
            };
            let Some(passed) = ctx.bool_flag(passed) else {
                return Err(invalid_test_foreign(foreign));
            };
            collector.cases.push(NativeTestCaseResult::new(
                suite_name(&collector.active_suites),
                name.as_str().into(),
                passed,
            ));
        }
        _ => return Err(invalid_test_foreign(foreign)),
    }
    Ok(())
}

fn invalid_test_foreign(foreign: &ForeignCall) -> VmError {
    VmError::new(VmErrorKind::ForeignCallRejected {
        foreign: foreign.name().into(),
    })
}

fn suite_name(path: &[Box<str>]) -> Box<str> {
    path.iter()
        .map(Box::as_ref)
        .collect::<Vec<_>>()
        .join(" / ")
        .into_boxed_str()
}
