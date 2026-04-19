use std::collections::HashMap;

use musi_vm::{EffectCall, ForeignCall, Value, VmHostCallContext, VmResult};

type ForeignHandler =
    Box<dyn FnMut(VmHostCallContext<'_, '_>, &ForeignCall, &[Value]) -> VmResult<Value>>;
type EffectHandler =
    Box<dyn FnMut(VmHostCallContext<'_, '_>, &EffectCall, &[Value]) -> VmResult<Value>>;
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
        mut handler: impl FnMut(&ForeignCall, &[Value]) -> VmResult<Value> + 'static,
    ) where
        Name: Into<HandlerName>,
    {
        self.register_foreign_handler_with_context(name, move |_ctx, foreign, args| {
            handler(foreign, args)
        });
    }

    pub fn register_foreign_handler_with_context<Name>(
        &mut self,
        name: Name,
        handler: impl FnMut(VmHostCallContext<'_, '_>, &ForeignCall, &[Value]) -> VmResult<Value>
        + 'static,
    ) where
        Name: Into<HandlerName>,
    {
        let _ = self.foreign_handlers.insert(name.into(), Box::new(handler));
    }

    pub fn register_effect_handler<Effect, Op>(
        &mut self,
        effect: Effect,
        op: Op,
        mut handler: impl FnMut(&EffectCall, &[Value]) -> VmResult<Value> + 'static,
    ) where
        Effect: Into<HandlerName>,
        Op: Into<HandlerName>,
    {
        self.register_effect_handler_with_context(effect, op, move |_ctx, effect, args| {
            handler(effect, args)
        });
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
        let _ = self
            .effect_handlers
            .insert((effect.into(), op.into()), Box::new(handler));
    }

    #[must_use]
    pub fn call_foreign(
        &mut self,
        ctx: VmHostCallContext<'_, '_>,
        foreign: &ForeignCall,
        args: &[Value],
    ) -> Option<VmResult<Value>> {
        let handler = self.foreign_handlers.get_mut(foreign.name())?;
        Some(handler(ctx, foreign, args))
    }

    #[must_use]
    pub fn handle_effect(
        &mut self,
        ctx: VmHostCallContext<'_, '_>,
        effect: &EffectCall,
        args: &[Value],
    ) -> Option<VmResult<Value>> {
        let handler = self
            .effect_handlers
            .get_mut(&(effect.effect_name().into(), effect.op_name().into()))?;
        Some(handler(ctx, effect, args))
    }
}
