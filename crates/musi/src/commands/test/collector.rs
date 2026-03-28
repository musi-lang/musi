use std::cell::RefCell;
use std::rc::Rc;

use musi_vm::{RuntimeHost, Value, Vm, VmError};

use super::decode::parse_event;
use super::model::{TestCase, TestEvent, TestHook, TestNode, TestSuite};

pub(super) struct TestCollector {
    stack: Vec<TestSuite>,
    pub(super) error: Option<String>,
}

pub(super) struct TestCollectorHandler {
    collector: Rc<RefCell<TestCollector>>,
    effect_id: u16,
    emit_op_id: u16,
}

impl TestCollector {
    pub(super) fn new(root_name: String) -> Self {
        Self {
            stack: vec![TestSuite {
                name: root_name,
                children: Vec::new(),
            }],
            error: None,
        }
    }

    pub(super) fn record_event(&mut self, vm: &Vm, value: Value) -> Result<(), String> {
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

    pub(super) fn finish(&mut self) -> Result<TestSuite, String> {
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

pub(super) fn new_handler(
    collector: Rc<RefCell<TestCollector>>,
    effect_id: u16,
    emit_op_id: u16,
) -> TestCollectorHandler {
    TestCollectorHandler {
        collector,
        effect_id,
        emit_op_id,
    }
}
