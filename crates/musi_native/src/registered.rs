use std::collections::HashMap;

use musi_vm::{EffectCall, ForeignCall, Value, VmResult};

type ForeignHandler = Box<dyn FnMut(&ForeignCall, &[Value]) -> VmResult<Value>>;
type EffectHandler = Box<dyn FnMut(&EffectCall, &[Value]) -> VmResult<Value>>;
type HandlerName = Box<str>;
type ForeignHandlerMap = HashMap<Box<str>, ForeignHandler>;
type EffectHandlerKey = (Box<str>, Box<str>);
type EffectHandlerMap = HashMap<EffectHandlerKey, EffectHandler>;

#[derive(Default)]
pub struct RegisteredHost {
    foreign_handlers: ForeignHandlerMap,
    effect_handlers: EffectHandlerMap,
}

impl RegisteredHost {
    pub fn register_foreign_handler<Name>(
        &mut self,
        name: Name,
        handler: impl FnMut(&ForeignCall, &[Value]) -> VmResult<Value> + 'static,
    ) where
        Name: Into<HandlerName>,
    {
        let _ = self.foreign_handlers.insert(name.into(), Box::new(handler));
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
        let _ = self
            .effect_handlers
            .insert((effect.into(), op.into()), Box::new(handler));
    }

    #[must_use]
    pub fn call_foreign(
        &mut self,
        foreign: &ForeignCall,
        args: &[Value],
    ) -> Option<VmResult<Value>> {
        let handler = self.foreign_handlers.get_mut(foreign.name())?;
        Some(handler(foreign, args))
    }

    #[must_use]
    pub fn handle_effect(
        &mut self,
        effect: &EffectCall,
        args: &[Value],
    ) -> Option<VmResult<Value>> {
        let handler = self
            .effect_handlers
            .get_mut(&(effect.effect_name().into(), effect.op_name().into()))?;
        Some(handler(effect, args))
    }
}
