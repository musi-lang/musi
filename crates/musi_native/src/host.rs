use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use musi_vm::{EffectCall, ForeignCall, Value, VmError, VmErrorKind, VmHost, VmResult};

type ForeignHandler = Box<dyn FnMut(&ForeignCall, &[Value]) -> VmResult<Value>>;
type EffectHandler = Box<dyn FnMut(&EffectCall, &[Value]) -> VmResult<Value>>;
type ForeignHandlerMap = HashMap<Box<str>, ForeignHandler>;
type EffectHandlerKey = (Box<str>, Box<str>);
type EffectHandlerMap = HashMap<EffectHandlerKey, EffectHandler>;

struct NativeHostState {
    fallback: Option<Box<dyn VmHost>>,
    foreign_handlers: ForeignHandlerMap,
    effect_handlers: EffectHandlerMap,
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
                foreign_handlers: ForeignHandlerMap::default(),
                effect_handlers: EffectHandlerMap::default(),
            })),
        }
    }

    #[must_use]
    pub fn with_fallback(host: impl VmHost + 'static) -> Self {
        Self {
            state: Rc::new(RefCell::new(NativeHostState {
                fallback: Some(Box::new(host)),
                foreign_handlers: ForeignHandlerMap::default(),
                effect_handlers: EffectHandlerMap::default(),
            })),
        }
    }

    pub fn register_foreign_handler(
        &mut self,
        name: impl Into<Box<str>>,
        handler: impl FnMut(&ForeignCall, &[Value]) -> VmResult<Value> + 'static,
    ) {
        let _ = self
            .state
            .borrow_mut()
            .foreign_handlers
            .insert(name.into(), Box::new(handler));
    }

    pub fn register_effect_handler(
        &mut self,
        effect: impl Into<Box<str>>,
        op: impl Into<Box<str>>,
        handler: impl FnMut(&EffectCall, &[Value]) -> VmResult<Value> + 'static,
    ) {
        let _ = self
            .state
            .borrow_mut()
            .effect_handlers
            .insert((effect.into(), op.into()), Box::new(handler));
    }

    fn call_registered_foreign(
        &self,
        foreign: &ForeignCall,
        args: &[Value],
    ) -> Option<VmResult<Value>> {
        let mut state = self.state.borrow_mut();
        let handler = state.foreign_handlers.get_mut(foreign.name())?;
        Some(handler(foreign, args))
    }

    fn call_registered_effect(
        &self,
        effect: &EffectCall,
        args: &[Value],
    ) -> Option<VmResult<Value>> {
        let mut state = self.state.borrow_mut();
        let handler = state
            .effect_handlers
            .get_mut(&(effect.effect_name().into(), effect.op_name().into()))?;
        Some(handler(effect, args))
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
        if let Some(result) = self.call_registered_foreign(foreign, args) {
            return result;
        }
        self.call_fallback(|host| host.call_foreign(foreign, args))
    }

    fn handle_effect(&mut self, effect: &EffectCall, args: &[Value]) -> VmResult<Value> {
        if let Some(result) = self.call_registered_effect(effect, args) {
            return result;
        }
        self.call_fallback(|host| host.handle_effect(effect, args))
    }
}
