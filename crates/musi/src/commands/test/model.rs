use musi_vm::Value;

#[derive(Clone)]
pub(super) struct TestSuite {
    pub(super) name: String,
    pub(super) children: Vec<TestNode>,
}

#[derive(Clone)]
pub(super) enum TestNode {
    Suite(TestSuite),
    Case(TestCase),
    Hook(TestHook),
}

#[derive(Clone)]
pub(super) struct TestCase {
    pub(super) name: String,
    pub(super) body: Value,
}

#[derive(Clone, Copy)]
pub(super) enum HookKind {
    BeforeAll,
    AfterAll,
    BeforeEach,
    AfterEach,
}

#[derive(Clone)]
pub(super) struct TestHook {
    pub(super) kind: HookKind,
    pub(super) body: Value,
}

pub(super) struct TestStats {
    pub(super) passed: usize,
    pub(super) failed: usize,
}

impl TestStats {
    pub(super) const fn new() -> Self {
        Self {
            passed: 0,
            failed: 0,
        }
    }
}

pub(super) enum Outcome {
    Pass,
    Fail(String),
}

pub(super) enum TestEvent {
    SuiteStart(String),
    SuiteEnd,
    Case(String, Value),
    Hook(HookKind, Value),
}
