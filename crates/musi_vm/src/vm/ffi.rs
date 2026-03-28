use super::*;
use crate::ffi::execute_ffi_call;

impl Vm {
    pub(super) fn dispatch_ffi_call(&mut self, method_idx: usize, pc: &mut usize) -> VmResult {
        let ffi_idx = usize::from(self.read_u16(method_idx, pc));
        let foreign = self
            .program
            .module()
            .foreigns
            .get(ffi_idx)
            .ok_or(VmError::FfiForeignIndexOutOfBounds(ffi_idx))?;

        let arity = usize::from(foreign.arity);
        let param_types = foreign.param_types.clone();
        let return_type = foreign.return_type;

        let symbol_name = if foreign.symbol_idx == u32::MAX {
            self.program
                .module()
                .strings
                .get(usize::try_from(foreign.name_idx).unwrap_or(usize::MAX))
                .cloned()
                .unwrap_or_default()
        } else {
            self.program
                .module()
                .strings
                .get(usize::try_from(foreign.symbol_idx).unwrap_or(usize::MAX))
                .cloned()
                .unwrap_or_default()
        };

        let lib_name = if foreign.lib_idx == u32::MAX {
            String::new()
        } else {
            self.program
                .module()
                .strings
                .get(usize::try_from(foreign.lib_idx).unwrap_or(usize::MAX))
                .cloned()
                .unwrap_or_default()
        };

        let mut host = self.host.take().ok_or(VmError::MissingHost)?;
        host.load_library(&lib_name)?;
        let fn_ptr = host.resolve_symbol(&lib_name, &symbol_name)?;
        self.host = Some(host);

        let mut args = Vec::with_capacity(arity);
        {
            let frame = self.frames.last_mut().ok_or(VmError::StackUnderflow)?;
            for _ in 0..arity {
                args.push(frame.pop()?);
            }
        }
        args.reverse();

        let result = execute_ffi_call(fn_ptr, &param_types, return_type, &mut args, &mut self.heap)?;
        self.push_stack(result)?;
        self.maybe_collect();
        Ok(())
    }
}
