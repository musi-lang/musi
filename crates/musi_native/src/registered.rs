use std::collections::HashMap;

use musi_vm::{ForeignCall, Value, VmHostCallContext, VmResult};

type ForeignHandler =
    Box<dyn FnMut(VmHostCallContext<'_, '_>, &ForeignCall, &[Value]) -> VmResult<Value> + Send>;
type HandlerName = Box<str>;
type ForeignHandlerMap = HashMap<Box<str>, ForeignHandler>;

#[derive(Default)]
pub struct RegisteredHost {
    foreign_handlers: ForeignHandlerMap,
}

impl RegisteredHost {
    pub fn register_foreign_handler<Name>(
        &mut self,
        name: Name,
        mut handler: impl FnMut(&ForeignCall, &[Value]) -> VmResult<Value> + Send + 'static,
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
        + Send
        + 'static,
    ) where
        Name: Into<HandlerName>,
    {
        let _ = self.foreign_handlers.insert(name.into(), Box::new(handler));
    }

    pub fn register_foundation_handler<Module, Op>(
        &mut self,
        module: Module,
        op: Op,
        mut handler: impl FnMut(&ForeignCall, &[Value]) -> VmResult<Value> + Send + 'static,
    ) where
        Module: AsRef<str>,
        Op: AsRef<str>,
    {
        self.register_foundation_handler_with_context(module, op, move |_ctx, foreign, args| {
            handler(foreign, args)
        });
    }

    pub fn register_foundation_handler_with_context<Module, Op>(
        &mut self,
        module: Module,
        op: Op,
        handler: impl FnMut(VmHostCallContext<'_, '_>, &ForeignCall, &[Value]) -> VmResult<Value>
        + Send
        + 'static,
    ) where
        Module: AsRef<str>,
        Op: AsRef<str>,
    {
        self.register_foreign_handler_with_context(foundation_foreign_name(module, op), handler);
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
}

fn foundation_foreign_name(module: impl AsRef<str>, op: impl AsRef<str>) -> Box<str> {
    let module = module
        .as_ref()
        .split_once("::")
        .map_or_else(|| module.as_ref(), |(head, _)| head);
    format!("{module}::Musi__{}", op.as_ref()).into_boxed_str()
}
