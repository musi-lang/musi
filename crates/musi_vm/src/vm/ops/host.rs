use std::mem::size_of;

use music_seam::{ForeignId, Instruction, Opcode, Operand, TypeId};

use crate::VmValueKind;

use super::{ForeignCall, StepOutcome, Value, Vm, VmError, VmErrorKind, VmResult};

impl Vm {
    pub(crate) fn foreign_call(&self, module_slot: usize, foreign_id: ForeignId) -> ForeignCall {
        let module = &self.loaded_modules[module_slot];
        let foreign = module.program.artifact().foreigns.get(foreign_id);
        ForeignCall {
            program: module.program.clone(),
            foreign: foreign_id,
            module: module.spec.clone(),
            name: module.program.string_text(foreign.name).into(),
            abi: module.program.string_text(foreign.abi).into(),
            symbol: module.program.string_text(foreign.symbol).into(),
            link: foreign
                .link
                .map(|link| module.program.string_text(link).into()),
            param_tys: foreign.param_tys.clone(),
            result_ty: foreign.result_ty,
        }
    }

    pub(crate) fn exec_host_edge(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        match instruction.opcode {
            Opcode::FfiCall => {
                let Operand::Foreign(foreign) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let module_slot = self.current_module_slot()?;
                let arg_len = self.loaded_modules[module_slot]
                    .program
                    .artifact()
                    .foreigns
                    .get(foreign)
                    .param_tys
                    .len();
                let args = self.pop_args(arg_len)?;
                let call = self.foreign_call(module_slot, foreign);
                let result = self
                    .call_musi_intrinsic(module_slot, &call, &args)
                    .unwrap_or_else(|| self.host.call_foreign(&call, &args))?;
                self.push_value(result)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::FfiCallSeq => {
                let Operand::Foreign(foreign) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let module_slot = self.current_module_slot()?;
                let args = self.pop_seq_args()?;
                let call = self.foreign_call(module_slot, foreign);
                let result = self
                    .call_musi_intrinsic(module_slot, &call, &args)
                    .unwrap_or_else(|| self.host.call_foreign(&call, &args))?;
                self.push_value(result)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::FfiRef => {
                let Operand::Foreign(foreign) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let module_slot = self.current_module_slot()?;
                self.push_value(Value::foreign(module_slot, foreign))?;
                Ok(StepOutcome::Continue)
            }
            Opcode::ModLoad => {
                let spec_value = self.pop_value()?;
                let spec = Self::expect_string_value(spec_value)?;
                let slot = self.load_dynamic_module(spec.as_ref())?;
                self.push_value(Value::module(spec.as_ref(), slot))?;
                Ok(StepOutcome::Continue)
            }
            Opcode::ModGet => {
                let Operand::String(name) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let module = self.pop_value()?;
                let module_slot = self.current_module_slot()?;
                let export_name = self
                    .module(module_slot)?
                    .program
                    .string_text(name)
                    .to_owned();
                let export = self.lookup_module_export(&module, &export_name)?;
                self.push_value(export)?;
                Ok(StepOutcome::Continue)
            }
            _ => Err(super::VmError::new(
                super::VmErrorKind::ProgramShapeInvalid {
                    detail: format!(
                        "host opcode family mismatch for `{}`",
                        instruction.opcode.mnemonic()
                    )
                    .into(),
                },
            )),
        }
    }

    pub(crate) fn specialize_foreign_call(
        mut foreign: ForeignCall,
        type_args: &[TypeId],
    ) -> ForeignCall {
        let Some(type_arg) = type_args.first().copied() else {
            return foreign;
        };
        if foreign.abi() != "musi" {
            return foreign;
        }
        let Some(suffix) = pointer_storage_suffix(foreign.type_name(type_arg)) else {
            return foreign;
        };
        let symbol = match foreign.symbol() {
            "offset" | "ffi.ptr.offset" => Some(format!("ffi.ptr.offset.{suffix}")),
            "read" | "ffi.ptr.read" => Some(format!("ffi.ptr.read.{suffix}")),
            "write" | "ffi.ptr.write" => Some(format!("ffi.ptr.write.{suffix}")),
            _ => None,
        };
        if let Some(symbol) = symbol {
            foreign.symbol = symbol.into();
        }
        foreign
    }

    pub(crate) fn call_musi_intrinsic(
        &self,
        module_slot: usize,
        foreign: &ForeignCall,
        args: &[Value],
    ) -> Option<VmResult<Value>> {
        if foreign.abi() != "musi" {
            return None;
        }
        let result = match foreign.symbol() {
            "data.tag" => Self::data_tag(foreign, args),
            "ffi.ptr.null" => Ok(Value::CPtr(0)),
            "ffi.ptr.is_null" => self.ptr_is_null(module_slot, foreign, args),
            "ffi.ptr.offset.i8" | "ffi.ptr.offset.u8" => Self::ptr_offset(foreign, args, 1),
            "ffi.ptr.offset.i16" | "ffi.ptr.offset.u16" => Self::ptr_offset(foreign, args, 2),
            "ffi.ptr.offset.i32" | "ffi.ptr.offset.u32" | "ffi.ptr.offset.f32" => {
                Self::ptr_offset(foreign, args, 4)
            }
            "ffi.ptr.offset.i64" | "ffi.ptr.offset.u64" | "ffi.ptr.offset.f64" => {
                Self::ptr_offset(foreign, args, 8)
            }
            "ffi.ptr.offset.ptr" => Self::ptr_offset(
                foreign,
                args,
                i64::try_from(size_of::<usize>()).unwrap_or(8),
            ),
            "ffi.ptr.size.i8" | "ffi.ptr.size.u8" => Ok(Value::Int(1)),
            "ffi.ptr.size.i16" | "ffi.ptr.size.u16" => Ok(Value::Int(2)),
            "ffi.ptr.size.i32" | "ffi.ptr.size.u32" | "ffi.ptr.size.f32" => Ok(Value::Int(4)),
            "ffi.ptr.size.i64" | "ffi.ptr.size.u64" | "ffi.ptr.size.f64" => Ok(Value::Int(8)),
            "ffi.ptr.size.ptr" => Ok(Value::Int(i64::try_from(size_of::<usize>()).unwrap_or(8))),
            _ => return None,
        };
        Some(result)
    }

    fn data_tag(foreign: &ForeignCall, args: &[Value]) -> VmResult<Value> {
        match args.first() {
            Some(Value::Data(data)) => Ok(Value::Int(data.borrow().tag)),
            Some(found) => Err(Self::invalid_value_kind(VmValueKind::Data, found)),
            None => Err(Self::ptr_error(foreign, "data tag argument missing")),
        }
    }

    fn ptr_is_null(
        &self,
        module_slot: usize,
        foreign: &ForeignCall,
        args: &[Value],
    ) -> VmResult<Value> {
        let address = Self::ptr_arg(foreign, args, 0)?;
        self.bool_value(module_slot, address == 0)
    }

    fn ptr_offset(foreign: &ForeignCall, args: &[Value], stride: i64) -> VmResult<Value> {
        let address = Self::ptr_arg(foreign, args, 0)?;
        let count = Self::int_arg(foreign, args, 1)?;
        let byte_count = count
            .checked_mul(stride)
            .ok_or_else(|| Self::ptr_error(foreign, "pointer offset overflow"))?;
        if address == 0 && count != 0 {
            return Err(Self::ptr_error(foreign, "null pointer offset"));
        }
        let next = if byte_count >= 0 {
            address.checked_add(usize::try_from(byte_count).map_err(|_| {
                Self::ptr_error(foreign, "pointer offset count exceeds address space")
            })?)
        } else {
            address.checked_sub(usize::try_from(byte_count.unsigned_abs()).map_err(|_| {
                Self::ptr_error(foreign, "pointer offset count exceeds address space")
            })?)
        };
        next.map(|address| Value::data(foreign.result_ty(), 0, [Value::CPtr(address)]))
            .ok_or_else(|| Self::ptr_error(foreign, "pointer offset overflow"))
    }

    fn ptr_arg(foreign: &ForeignCall, args: &[Value], index: usize) -> VmResult<usize> {
        match args.get(index) {
            Some(Value::CPtr(address)) => Ok(*address),
            Some(Value::Data(data)) => {
                let data = data.borrow();
                match data.fields.first() {
                    Some(Value::CPtr(address)) => Ok(*address),
                    Some(found) => Err(Self::invalid_value_kind(VmValueKind::CPtr, found)),
                    None => Err(Self::ptr_error(foreign, "pointer field missing")),
                }
            }
            Some(found) => Err(Self::invalid_value_kind(VmValueKind::CPtr, found)),
            None => Err(Self::ptr_error(
                foreign,
                "pointer intrinsic argument missing",
            )),
        }
    }

    fn int_arg(foreign: &ForeignCall, args: &[Value], index: usize) -> VmResult<i64> {
        match args.get(index) {
            Some(Value::Int(value)) => Ok(*value),
            Some(found) => Err(Self::invalid_value_kind(VmValueKind::Int, found)),
            None => Err(Self::ptr_error(
                foreign,
                "pointer intrinsic argument missing",
            )),
        }
    }

    fn ptr_error(foreign: &ForeignCall, detail: &'static str) -> VmError {
        VmError::new(VmErrorKind::PointerIntrinsicFailed {
            intrinsic: foreign.symbol().into(),
            detail: detail.into(),
        })
    }
}

fn pointer_storage_suffix(type_name: &str) -> Option<&'static str> {
    const POINTER_STORAGE_SUFFIXES: &[(&str, &str)] = &[
        ("CChar", "i8"),
        ("CSChar", "i8"),
        ("CUChar", "u8"),
        ("CShort", "i16"),
        ("CUShort", "u16"),
        ("CInt", "i32"),
        ("CUInt", "u32"),
        ("Int8", "i8"),
        ("Nat8", "u8"),
        ("Int16", "i16"),
        ("Nat16", "u16"),
        ("Int32", "i32"),
        ("Nat32", "u32"),
        ("Int64", "i64"),
        ("Nat64", "u64"),
        ("CLong", "i64"),
        ("CLongLong", "i64"),
        ("CSizeDiff", "i64"),
        ("Int", "i64"),
        ("Nat", "u64"),
        ("CULong", "u64"),
        ("CULongLong", "u64"),
        ("CSize", "u64"),
        ("CFloat", "f32"),
        ("Float32", "f32"),
        ("CDouble", "f64"),
        ("Float64", "f64"),
        ("Float", "f64"),
        ("CPtr", "ptr"),
    ];
    POINTER_STORAGE_SUFFIXES
        .iter()
        .find_map(|(name, suffix)| (*name == type_name).then_some(*suffix))
}
