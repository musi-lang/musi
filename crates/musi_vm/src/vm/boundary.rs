use super::{
    EffectCall, ForeignCall, Program, RejectingHost, RejectingLoader, Value, Vm, VmHost,
    VmHostContext, VmLoader, VmResult,
};

pub(super) enum LoaderState {
    Rejecting(RejectingLoader),
    Custom(Box<dyn VmLoader>),
}

impl LoaderState {
    pub(super) fn load_program(&mut self, spec: &str) -> VmResult<Program> {
        match self {
            Self::Rejecting(loader) => loader.load_program(spec),
            Self::Custom(loader) => loader.load_program(spec),
        }
    }
}

pub(super) enum HostState {
    Rejecting(RejectingHost),
    Custom(Box<dyn VmHost>),
}

impl HostState {
    pub(super) fn call_foreign(
        &mut self,
        ctx: &mut VmHostContext<'_>,
        foreign: &ForeignCall,
        args: &[Value],
    ) -> VmResult<Value> {
        match self {
            Self::Rejecting(host) => host.call_foreign(ctx, foreign, args),
            Self::Custom(host) => host.call_foreign(ctx, foreign, args),
        }
    }

    pub(super) fn handle_effect(
        &mut self,
        ctx: &mut VmHostContext<'_>,
        effect: &EffectCall,
        args: &[Value],
    ) -> VmResult<Value> {
        match self {
            Self::Rejecting(host) => host.handle_effect(ctx, effect, args),
            Self::Custom(host) => host.handle_effect(ctx, effect, args),
        }
    }
}

impl Vm {
    pub(crate) fn call_host_foreign(
        &mut self,
        foreign: &ForeignCall,
        args: &[Value],
    ) -> VmResult<Value> {
        let options = self.heap_options();
        let mut ctx = VmHostContext::new(&mut self.heap, options);
        let result = self.host.call_foreign(&mut ctx, foreign, args)?;
        self.after_host_call_result(&result)?;
        Ok(result)
    }

    pub(crate) fn call_host_effect(
        &mut self,
        effect: &EffectCall,
        args: &[Value],
    ) -> VmResult<Value> {
        let options = self.heap_options();
        let mut ctx = VmHostContext::new(&mut self.heap, options);
        let result = self.host.handle_effect(&mut ctx, effect, args)?;
        self.after_host_call_result(&result)?;
        Ok(result)
    }
}
