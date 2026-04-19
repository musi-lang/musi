use std::cell::RefCell;
use std::rc::{Rc, Weak};

use musi_vm::{
    EffectCall, ForeignCall, Value, VmError, VmErrorKind, VmHost, VmHostCallContext, VmResult,
};

use crate::platform::PlatformHost;
use crate::registered::RegisteredHost;
use crate::testing::{NativeTestReport, TestHost};

type HandlerName = Box<str>;

struct NativeHostState {
    fallback: Option<Box<dyn VmHost>>,
    registered: RegisteredHost,
    testing: TestHost,
    platform: PlatformHost,
}

#[derive(Clone)]
pub struct WeakNativeHost {
    state: Weak<RefCell<NativeHostState>>,
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

    pub fn register_foreign_handler<Name>(
        &mut self,
        name: Name,
        handler: impl FnMut(&ForeignCall, &[Value]) -> VmResult<Value> + 'static,
    ) where
        Name: Into<HandlerName>,
    {
        self.state
            .borrow_mut()
            .registered
            .register_foreign_handler(name, handler);
    }

    pub fn register_foreign_handler_with_context<Name>(
        &mut self,
        name: Name,
        handler: impl FnMut(VmHostCallContext<'_, '_>, &ForeignCall, &[Value]) -> VmResult<Value>
        + 'static,
    ) where
        Name: Into<HandlerName>,
    {
        self.state
            .borrow_mut()
            .registered
            .register_foreign_handler_with_context(name, handler);
    }

    pub fn register_effect_handler<Effect, Op>(
        &mut self,
        effect: Effect,
        op: Op,
        handler: impl FnMut(&EffectCall, &[Value]) -> VmResult<Value> + 'static,
    ) where
        Effect: Into<HandlerName>,
        Op: Into<HandlerName>,
    {
        self.state
            .borrow_mut()
            .registered
            .register_effect_handler(effect, op, handler);
    }

    pub fn register_effect_handler_with_context<Effect, Op>(
        &mut self,
        effect: Effect,
        op: Op,
        handler: impl FnMut(VmHostCallContext<'_, '_>, &EffectCall, &[Value]) -> VmResult<Value>
        + 'static,
    ) where
        Effect: Into<HandlerName>,
        Op: Into<HandlerName>,
    {
        self.state
            .borrow_mut()
            .registered
            .register_effect_handler_with_context(effect, op, handler);
    }

    pub fn begin_test_session(&mut self) {
        self.state.borrow_mut().testing.begin_session();
    }

    #[must_use]
    pub fn finish_test_session(&mut self, module: &str) -> NativeTestReport {
        self.state.borrow_mut().testing.finish_session(module)
    }

    #[must_use]
    pub fn downgrade(&self) -> WeakNativeHost {
        WeakNativeHost {
            state: Rc::downgrade(&self.state),
        }
    }

    fn call_fallback<R>(&self, f: impl FnOnce(&mut dyn VmHost) -> VmResult<R>) -> VmResult<R> {
        let mut state = self.state.borrow_mut();
        let Some(fallback) = state.fallback.as_mut() else {
            return Err(VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "native host fallback missing".into(),
            }));
        };
        f(fallback.as_mut())
    }
}

impl WeakNativeHost {
    #[must_use]
    pub fn upgrade(&self) -> Option<NativeHost> {
        self.state.upgrade().map(|state| NativeHost { state })
    }
}

impl VmHost for NativeHost {
    fn call_foreign(
        &mut self,
        ctx: VmHostCallContext<'_, '_>,
        foreign: &ForeignCall,
        args: &[Value],
    ) -> VmResult<Value> {
        if let Some(result) = self
            .state
            .borrow_mut()
            .registered
            .call_foreign(ctx, foreign, args)
        {
            return result;
        }
        if let Some(result) = self
            .state
            .borrow()
            .platform
            .call_foreign(ctx, foreign, args)
        {
            return result;
        }
        if self.state.borrow().fallback.is_none() {
            return Err(VmError::new(VmErrorKind::ForeignCallRejected {
                foreign: foreign.name().into(),
            }));
        }
        self.call_fallback(|host| host.call_foreign(ctx, foreign, args))
    }

    fn handle_effect(
        &mut self,
        ctx: VmHostCallContext<'_, '_>,
        effect: &EffectCall,
        args: &[Value],
    ) -> VmResult<Value> {
        if let Some(result) = self
            .state
            .borrow_mut()
            .registered
            .handle_effect(ctx, effect, args)
        {
            return result;
        }
        if let Some(result) = self
            .state
            .borrow_mut()
            .testing
            .handle_effect(ctx, effect, args)
        {
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
        self.call_fallback(|host| host.handle_effect(ctx, effect, args))
    }
}
