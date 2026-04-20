use std::sync::{Arc, Mutex, MutexGuard, Weak};

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
    state: Weak<Mutex<NativeHostState>>,
}

#[derive(Clone)]
pub struct NativeHost {
    state: Arc<Mutex<NativeHostState>>,
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
            state: Arc::new(Mutex::new(NativeHostState {
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
            state: Arc::new(Mutex::new(NativeHostState {
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
        handler: impl FnMut(&ForeignCall, &[Value]) -> VmResult<Value> + Send + 'static,
    ) where
        Name: Into<HandlerName>,
    {
        if let Ok(mut state) = self.state.lock() {
            state.registered.register_foreign_handler(name, handler);
        }
    }

    pub fn register_foreign_handler_with_context<Name>(
        &mut self,
        name: Name,
        handler: impl FnMut(VmHostCallContext<'_, '_>, &ForeignCall, &[Value]) -> VmResult<Value>
        + Send
        + 'static,
    ) where
        Name: Into<HandlerName>,
    {
        if let Ok(mut state) = self.state.lock() {
            state
                .registered
                .register_foreign_handler_with_context(name, handler);
        }
    }

    pub fn register_effect_handler<Effect, Op>(
        &mut self,
        effect: Effect,
        op: Op,
        handler: impl FnMut(&EffectCall, &[Value]) -> VmResult<Value> + Send + 'static,
    ) where
        Effect: Into<HandlerName>,
        Op: Into<HandlerName>,
    {
        if let Ok(mut state) = self.state.lock() {
            state
                .registered
                .register_effect_handler(effect, op, handler);
        }
    }

    pub fn register_effect_handler_with_context<Effect, Op>(
        &mut self,
        effect: Effect,
        op: Op,
        handler: impl FnMut(VmHostCallContext<'_, '_>, &EffectCall, &[Value]) -> VmResult<Value>
        + Send
        + 'static,
    ) where
        Effect: Into<HandlerName>,
        Op: Into<HandlerName>,
    {
        if let Ok(mut state) = self.state.lock() {
            state
                .registered
                .register_effect_handler_with_context(effect, op, handler);
        }
    }

    pub fn begin_test_session(&mut self) {
        if let Ok(mut state) = self.state.lock() {
            state.testing.begin_session();
        }
    }

    #[must_use]
    pub fn finish_test_session(&mut self, module: &str) -> NativeTestReport {
        self.state.lock().map_or_else(
            |_| NativeTestReport::new(module, Box::default()),
            |mut state| state.testing.finish_session(module),
        )
    }

    #[must_use]
    pub fn downgrade(&self) -> WeakNativeHost {
        WeakNativeHost {
            state: Arc::downgrade(&self.state),
        }
    }

    fn state(&self) -> VmResult<MutexGuard<'_, NativeHostState>> {
        self.state.lock().map_err(|_| {
            VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "native host lock poisoned".into(),
            })
        })
    }

    #[allow(clippy::significant_drop_tightening)]
    fn call_fallback<R>(&self, f: impl FnOnce(&mut dyn VmHost) -> VmResult<R>) -> VmResult<R> {
        let mut state = self.state()?;
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
        let registered_result = self.state()?.registered.call_foreign(ctx, foreign, args);
        if let Some(result) = registered_result {
            return result;
        }

        let platform_result = self.state()?.platform.call_foreign(ctx, foreign, args);
        if let Some(result) = platform_result {
            return result;
        }

        if self.state()?.fallback.is_none() {
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
        let registered_result = self.state()?.registered.handle_effect(ctx, effect, args);
        if let Some(result) = registered_result {
            return result;
        }

        let testing_result = self.state()?.testing.handle_effect(ctx, effect, args);
        if let Some(result) = testing_result {
            return result;
        }

        let platform_result = self.state()?.platform.handle_effect(effect, args);
        if let Some(result) = platform_result {
            return result;
        }

        if self.state()?.fallback.is_none() {
            return Err(VmError::new(VmErrorKind::EffectRejected {
                effect: effect.effect_name().into(),
                op: Some(effect.op_name().into()),
                reason: "native host rejected runtime effect".into(),
            }));
        }
        self.call_fallback(|host| host.handle_effect(ctx, effect, args))
    }
}
