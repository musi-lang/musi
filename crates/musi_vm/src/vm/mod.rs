// Performance-critical hot paths use unsafe for unchecked indexing
#![allow(clippy::arithmetic_side_effects, clippy::as_conversions, unsafe_code)]

mod branch;
mod array;
mod call;
mod effects;
mod ffi;
mod gc;
mod scalar;
mod types;

use music_il::format;
use music_il::opcode::Opcode;

use crate::effect::EffectHandler;
use crate::errors::{VmError, VmResult};
use crate::frame::CallFrame;
use crate::heap::{Heap, HeapObject};
use crate::host::{NativeHost, RuntimeHost};
use crate::inspect::{ArrayValue, ValueView};
use crate::module::{ConstantEntry, ENTRY_POINT_NAME};
use crate::program::Program;
use crate::value::Value;

const MAX_CALL_DEPTH: usize = 1024;

#[cfg(test)]
pub(crate) use scalar::display_value;

pub struct Vm {
    program: Program,
    globals: Vec<Value>,
    heap: Heap,
    frames: Vec<CallFrame>,
    effect_handlers: Vec<EffectHandler>,
    host: Option<Box<dyn RuntimeHost>>,
    resolved_constants: Vec<Value>,
    initialized: bool,
    last_init_result: Value,
}

impl Vm {
    #[must_use]
    pub fn new(program: impl Into<Program>) -> Self {
        Self::with_host(program, Box::new(NativeHost::new()))
    }

    #[must_use]
    pub fn with_host(program: impl Into<Program>, host: Box<dyn RuntimeHost>) -> Self {
        let program = program.into();
        let module = program.module();
        let globals_count = module.globals.len();
        let mut heap = Heap::new();
        let resolved_constants = module
            .constants
            .iter()
            .map(|entry| match entry {
                ConstantEntry::Value(v) => *v,
                ConstantEntry::StringRef(idx) => {
                    let s = module
                        .strings
                        .get(usize::from(*idx))
                        .cloned()
                        .unwrap_or_default();
                    Value::from_ptr(heap.alloc_string(s))
                }
            })
            .collect();
        Self {
            program,
            globals: vec![Value::UNIT; globals_count],
            heap,
            frames: Vec::new(),
            effect_handlers: Vec::new(),
            host: Some(host),
            resolved_constants,
            initialized: false,
            last_init_result: Value::UNIT,
        }
    }

    #[must_use]
    pub const fn program(&self) -> &Program {
        &self.program
    }

    #[must_use]
    pub const fn is_initialized(&self) -> bool {
        self.initialized
    }

    /// # Errors
    /// Returns a [`VmError`] if no entry point exists, the bytecode is invalid,
    /// a runtime type error occurs, or `Panic` is executed.
    pub fn initialize(&mut self) -> VmResult {
        if self.initialized {
            return Ok(());
        }

        let entry_indices: Vec<_> = self
            .program
            .module()
            .methods
            .iter()
            .enumerate()
            .filter_map(|(idx, method)| (method.name == ENTRY_POINT_NAME).then_some(idx))
            .collect();
        if entry_indices.is_empty() {
            return Err(VmError::NoEntryPoint);
        }

        let mut result = Value::UNIT;
        for entry_idx in entry_indices {
            self.frames.clear();
            let locals_count = usize::from(self.program.module().methods[entry_idx].locals_count);
            let frame = CallFrame::new_call(
                locals_count,
                0,
                u16::try_from(entry_idx).map_err(|_| VmError::InvalidMethod(entry_idx))?,
                None,
            );
            self.frames.push(frame);
            result = self.execute()?;
        }
        self.frames.clear();
        self.last_init_result = result;
        self.initialized = true;
        Ok(())
    }

    /// # Errors
    /// Returns a [`VmError`] if no entry point exists, the bytecode is invalid,
    /// a runtime type error occurs, or `Panic` is executed.
    pub fn run(&mut self) -> VmResult<Value> {
        self.initialize()?;
        Ok(self.last_init_result)
    }

    /// Execute a callable value already stored in the VM.
    ///
    /// The module entrypoint must have been run first if the callee depends on
    /// initialized globals.
    pub fn invoke(&mut self, callee: Value, args: &[Value]) -> VmResult<Value> {
        if !self.initialized {
            return Err(VmError::ProgramNotInitialized);
        }
        self.frames.clear();
        let (method_idx, closure_idx) = self.resolve_callee(callee)?;
        let mut frame = self.make_call_frame(method_idx, 0, closure_idx)?;
        for (i, arg) in args.iter().enumerate() {
            frame.store_local(i, *arg)?;
        }
        self.frames.push(frame);
        let result = self.execute();
        self.frames.clear();
        result
    }

    /// Look up an exported global by source name after module initialization.
    #[must_use]
    pub fn export(&self, name: &str) -> Option<Value> {
        if !self.initialized {
            return None;
        }

        self.program
            .module()
            .globals
            .iter()
            .enumerate()
            .find(|(_, global)| {
                global.exported
                    && self
                        .program
                        .module()
                        .strings
                        .get(usize::try_from(global.name).ok().unwrap_or(usize::MAX))
                        .is_some_and(|global_name| global_name == name)
            })
            .and_then(|(idx, _)| self.globals.get(idx).copied())
    }

    #[doc(hidden)]
    #[must_use]
    pub fn global_value(&self, index: u16) -> Option<Value> {
        if !self.initialized {
            return None;
        }
        self.globals.get(usize::from(index)).copied()
    }

    /// Invoke an exported callable by name after initialization.
    pub fn invoke_export(&mut self, name: &str, args: &[Value]) -> VmResult<Value> {
        let callee = self
            .export(name)
            .ok_or_else(|| VmError::MissingExport(name.into()))?;
        self.invoke(callee, args)
    }

    /// Decode a runtime value into a host-owned view.
    #[must_use]
    pub fn inspect(&self, value: Value) -> Option<ValueView> {
        if value.is_unit() {
            return Some(ValueView::Unit);
        }
        if value.is_bool() {
            return Some(ValueView::Bool(value.as_bool()));
        }
        if value.is_int() {
            return Some(ValueView::Int(value.as_int()));
        }
        if value.is_float() {
            return Some(ValueView::Float(value.as_float()));
        }
        if value.is_tag() {
            return Some(ValueView::Tag(value.as_tag_idx()));
        }
        if !value.is_ptr() {
            return None;
        }

        match self.heap.get(value.as_ptr_idx())? {
            HeapObject::String(s) => Some(ValueView::String(s.data.clone())),
            HeapObject::Array(arr) => Some(ValueView::Array(ArrayValue {
                tag: arr.tag,
                elements: arr.elements.clone(),
            })),
            HeapObject::Slice(sl) => Some(ValueView::Slice {
                length: sl.end - sl.start,
            }),
            HeapObject::Cell(cell) => Some(ValueView::Cell(cell.value)),
            HeapObject::CPtr(_) => Some(ValueView::CPtr),
            HeapObject::Closure(_) => Some(ValueView::Closure),
            HeapObject::Continuation(_) => Some(ValueView::Continuation),
        }
    }

    fn execute(&mut self) -> VmResult<Value> {
        let mut pc: usize = 0;
        loop {
            // SAFETY: execute() is only called from run() which pushes an
            // initial frame, and every Ret that pops the last frame returns
            // before we reach this point. Frames is therefore non-empty.
            // SAFETY: as_mut_ptr() is safe; .add() is unsafe but frames is non-empty
            let frame_ptr = unsafe { self.frames.as_mut_ptr().add(self.frames.len() - 1) };
            // SAFETY: the pointer is valid because frames is non-empty (see above)
            let frame = unsafe { &mut *frame_ptr };
            let method_idx = usize::from(frame.method_idx);
            // SAFETY: method_idx is set from valid module data during frame
            // creation; pc is maintained within bounds by the emitter.
            let code = unsafe { &self.program.module().methods.get_unchecked(method_idx).code };
            let byte = unsafe { *code.get_unchecked(pc) };
            // SAFETY: emitter only produces valid opcodes
            let op = unsafe { Opcode::from_byte(byte).unwrap_unchecked() };
            pc = pc.wrapping_add(1);

            match op {
                Opcode::Halt => {
                    return Ok(frame.peek_or(Value::UNIT));
                }
                Opcode::Nop => {}
                Opcode::Panic => return Err(VmError::ExplicitPanic),

                Opcode::BrTrue
                | Opcode::BrFalse
                | Opcode::BrJmp
                | Opcode::BrBack
                | Opcode::BrTbl => {
                    self.dispatch_branch(op, method_idx, &mut pc)?;
                }

                Opcode::Ret | Opcode::Call | Opcode::CallTail | Opcode::ClsNew => {
                    if let Some(ret) = self.dispatch_call(op, method_idx, &mut pc)? {
                        return Ok(ret);
                    }
                }

                Opcode::EffHdlPush | Opcode::EffHdlPop | Opcode::EffInvk | Opcode::EffCont => {
                    self.dispatch_effect(op, method_idx, &mut pc)?;
                }

                Opcode::ArrNew
                | Opcode::ArrNewT
                | Opcode::ArrGet
                | Opcode::ArrSet
                | Opcode::ArrGetI
                | Opcode::ArrSetI
                | Opcode::ArrLen
                | Opcode::ArrTag
                | Opcode::ArrCopy
                | Opcode::ArrCaten => {
                    self.dispatch_array(op, method_idx, &mut pc)?;
                }

                Opcode::TyChk | Opcode::TyCast | Opcode::TyTag => {
                    self.dispatch_type_op(op, method_idx, &mut pc)?;
                }

                Opcode::TyclDict | Opcode::TyclCall => {
                    self.dispatch_tycl(op, method_idx, &mut pc)?;
                }

                Opcode::FfiCall => {
                    self.dispatch_ffi_call(method_idx, &mut pc)?;
                }

                Opcode::ArrSlice => {
                    self.exec_arr_slice()?;
                }

                Opcode::ArrFill => {
                    self.exec_arr_fill()?;
                }

                Opcode::CmpEq | Opcode::CmpNeq => {
                    self.dispatch_equality(op)?;
                }

                Opcode::GcPin => {
                    let val = self.pop_stack()?;
                    if val.is_ptr() {
                        self.heap.pin(val.as_ptr_idx());
                    }
                    self.push_stack(val)?;
                }

                Opcode::GcUnpin => {
                    let val = self.pop_stack()?;
                    if val.is_ptr() {
                        self.heap.unpin(val.as_ptr_idx());
                    }
                    self.push_stack(val)?;
                }

                _ => self.dispatch_load_store_stack(op, method_idx, &mut pc)?,
            }
        }
    }

}

#[cfg(test)]
mod tests;
