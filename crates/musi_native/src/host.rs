use std::cell::RefCell;
use std::rc::Rc;

use musi_vm::{EffectCall, ForeignCall, Value, VmError, VmErrorKind, VmHost, VmResult};

use crate::platform::PlatformHost;
use crate::registered::RegisteredHost;
use crate::testing::{NativeTestReport, TestHost};

struct NativeHostState {
    fallback: Option<Box<dyn VmHost>>,
    registered: RegisteredHost,
    testing: TestHost,
    platform: PlatformHost,
}

#[derive(Clone)]
pub struct NativeHost {
    state: Rc<RefCell<NativeHostState>>,
}

impl Default for NativeHost {
    fn default() -> Self {
        Self::new()
    }
}

impl NativeHost {
    #[must_use]
    pub fn new() -> Self {
        Self {
            state: Rc::new(RefCell::new(NativeHostState {
                fallback: None,
                registered: RegisteredHost::default(),
                testing: TestHost::default(),
                platform: PlatformHost::new(),
            })),
        }
    }

    #[must_use]
    pub fn with_fallback(host: impl VmHost + 'static) -> Self {
        Self {
            state: Rc::new(RefCell::new(NativeHostState {
                fallback: Some(Box::new(host)),
                registered: RegisteredHost::default(),
                testing: TestHost::default(),
                platform: PlatformHost::new(),
            })),
        }
    }

    pub fn register_foreign_handler(
        &mut self,
        name: impl Into<Box<str>>,
        handler: impl FnMut(&ForeignCall, &[Value]) -> VmResult<Value> + 'static,
    ) {
        self.state
            .borrow_mut()
            .registered
            .register_foreign_handler(name, handler);
    }

    pub fn register_effect_handler(
        &mut self,
        effect: impl Into<Box<str>>,
        op: impl Into<Box<str>>,
        handler: impl FnMut(&EffectCall, &[Value]) -> VmResult<Value> + 'static,
    ) {
        self.state
            .borrow_mut()
            .registered
            .register_effect_handler(effect, op, handler);
    }

    pub fn begin_test_session(&mut self) {
        self.state.borrow_mut().testing.begin_session();
    }

    #[must_use]
    pub fn finish_test_session(&mut self, module: &str) -> NativeTestReport {
        self.state.borrow_mut().testing.finish_session(module)
    }

    fn call_fallback<R>(&self, f: impl FnOnce(&mut dyn VmHost) -> VmResult<R>) -> VmResult<R> {
        let mut state = self.state.borrow_mut();
        let Some(fallback) = state.fallback.as_mut() else {
            return Err(VmError::new(VmErrorKind::InvalidProgram {
                detail: "native host fallback missing".into(),
            }));
        };
        f(fallback.as_mut())
    }
}

impl VmHost for NativeHost {
    fn call_foreign(&mut self, foreign: &ForeignCall, args: &[Value]) -> VmResult<Value> {
        if let Some(result) = self
            .state
            .borrow_mut()
            .registered
            .call_foreign(foreign, args)
        {
            return result;
        }
        if let Some(result) = self.state.borrow().platform.call_foreign(foreign, args) {
            return result;
        }
        if self.state.borrow().fallback.is_none() {
            return Err(VmError::new(VmErrorKind::ForeignCallRejected {
                foreign: foreign.name().into(),
            }));
        }
        self.call_fallback(|host| host.call_foreign(foreign, args))
    }

    fn handle_effect(&mut self, effect: &EffectCall, args: &[Value]) -> VmResult<Value> {
        if let Some(result) = self
            .state
            .borrow_mut()
            .registered
            .handle_effect(effect, args)
        {
            return result;
        }
        if let Some(result) = self.state.borrow_mut().testing.handle_effect(effect, args) {
            return result;
        }
        if let Some(result) = self.state.borrow().platform.handle_effect(effect, args) {
            return result;
        }
        if self.state.borrow().fallback.is_none() {
            return Err(VmError::new(VmErrorKind::EffectRejected {
                effect: effect.effect_name().into(),
                op: Some(effect.op_name().into()),
                reason: "native host rejected runtime effect".into(),
            }));
        }
        self.call_fallback(|host| host.handle_effect(effect, args))
    }
}
