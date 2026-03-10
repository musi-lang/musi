//! Main emitter orchestrator: drives per-function emission and assembles the module.

#[cfg(test)]
mod tests;

mod control;
mod fn_emitter;
mod rvalue;

use music_ir::{IrFnIdx, IrFunction, IrModule};
use music_shared::Interner;

use crate::const_pool::ConstPool;
use crate::error::EmitError;
use crate::type_pool::TypePool;

use fn_emitter::{FnEmitter, HandlerEntry};

/// Per-function bytecode output assembled by the emitter.
pub struct FnBytecode {
    pub fn_id: u32,
    pub type_id: u32,
    pub local_count: u16,
    pub param_count: u16,
    pub max_stack: u16,
    pub effect_mask: u16,
    pub code: Vec<u8>,
    pub handlers: Vec<HandlerEntry>,
}

/// Orchestrates the full module emission.
pub struct Emitter<'a> {
    module: &'a IrModule,
    interner: &'a Interner,
    pub cp: ConstPool,
    pub tp: TypePool,
}

impl<'a> Emitter<'a> {
    pub fn new(module: &'a IrModule, interner: &'a Interner) -> Self {
        Self {
            module,
            interner,
            cp: ConstPool::new(),
            tp: TypePool::new(),
        }
    }

    /// Emit all functions and return their bytecode records.
    pub fn emit_functions(&mut self) -> Result<Vec<FnBytecode>, EmitError> {
        let mut results = vec![];
        for fn_idx in function_indices(self.module) {
            let func = &self.module.functions[fn_idx];
            let fn_bc = self.emit_function(func)?;
            results.push(fn_bc);
        }
        Ok(results)
    }

    fn emit_function(&mut self, func: &IrFunction) -> Result<FnBytecode, EmitError> {
        let param_count =
            u16::try_from(func.params.len()).map_err(|_| EmitError::UnresolvableType {
                desc: "too many params".into(),
            })?;
        let local_count =
            u16::try_from(func.locals.len()).map_err(|_| EmitError::UnresolvableType {
                desc: "too many locals".into(),
            })?;

        let mut fe = FnEmitter::new(param_count, local_count);

        let fn_name = self.interner.resolve(func.name).to_owned();

        for inst in &func.body {
            let _terminated = control::emit_inst(
                &mut fe,
                &mut self.cp,
                &mut self.tp,
                &self.module.types,
                inst,
                self.interner,
            )?;
        }

        fe.resolve_fixups(&fn_name)?;
        let _code_len = fe.validate_code_len()?;

        let fn_id = func.id.0;
        let type_id = self.tp.lower_ir_type(func.ret_ty, &self.module.types)?;
        let effect_mask = func.effects.0;

        Ok(FnBytecode {
            fn_id,
            type_id,
            local_count: fe.local_count,
            param_count: fe.param_count,
            max_stack: fe.max_stack,
            effect_mask,
            code: fe.code,
            handlers: fe.handlers,
        })
    }
}

/// Serialize the function pool section into `buf`.
pub fn write_function_pool(buf: &mut Vec<u8>, functions: &[FnBytecode]) -> Result<(), EmitError> {
    let count = u32::try_from(functions.len()).map_err(|_| EmitError::UnresolvableType {
        desc: "too many functions".into(),
    })?;
    buf.extend_from_slice(&count.to_le_bytes());
    for fn_bc in functions {
        buf.extend_from_slice(&fn_bc.fn_id.to_le_bytes());
        buf.extend_from_slice(&fn_bc.type_id.to_le_bytes());
        buf.extend_from_slice(&fn_bc.local_count.to_le_bytes());
        buf.extend_from_slice(&fn_bc.param_count.to_le_bytes());
        buf.extend_from_slice(&fn_bc.max_stack.to_le_bytes());
        buf.extend_from_slice(&fn_bc.effect_mask.to_le_bytes());
        let code_len = u32::try_from(fn_bc.code.len()).map_err(|_| EmitError::FunctionTooLarge)?;
        buf.extend_from_slice(&code_len.to_le_bytes());
        buf.extend_from_slice(&fn_bc.code);
        // Handler table
        let handler_count =
            u16::try_from(fn_bc.handlers.len()).map_err(|_| EmitError::OperandOverflow {
                desc: "too many handler entries".into(),
            })?;
        buf.extend_from_slice(&handler_count.to_le_bytes());
        for h in &fn_bc.handlers {
            buf.push(h.effect_id);
            buf.extend_from_slice(&h.handler_fn_id.to_le_bytes());
        }
    }
    Ok(())
}

fn function_indices(module: &IrModule) -> impl Iterator<Item = IrFnIdx> {
    let count = u32::try_from(module.functions.len()).expect("function count fits u32");
    (0..count).map(IrFnIdx::from_raw)
}
