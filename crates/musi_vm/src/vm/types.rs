use super::*;

impl Vm {
    pub(super) fn dispatch_type_op(
        &mut self,
        op: Opcode,
        method_idx: usize,
        pc: &mut usize,
    ) -> VmResult {
        match op {
            Opcode::TyChk => {
                let type_id = self.read_u16(method_idx, pc);
                let val = self.pop_stack()?;
                let matches = self.value_matches_type(val, type_id);
                self.push_stack(Value::from_bool(matches))?;
            }
            Opcode::TyCast => {
                let type_id = self.read_u16(method_idx, pc);
                let val = self.pop_stack()?;
                if self.value_matches_type(val, type_id) {
                    self.push_stack(val)?;
                } else {
                    return Err(VmError::TypeCastFailed);
                }
            }
            Opcode::TyTag => {
                let val = self.pop_stack()?;
                self.push_stack(Value::from_int(i64::from(val.nan_tag())))?;
            }
            _ => return Err(VmError::UnsupportedOpcode(op)),
        }
        Ok(())
    }

    pub(super) fn runtime_type_id_of_value(&self, val: Value) -> Option<u16> {
        if val.is_int() {
            return Some(format::BUILTIN_TYPE_INT);
        }
        if val.is_bool() {
            return Some(format::BUILTIN_TYPE_BOOL);
        }
        if val.is_unit() {
            return Some(format::BUILTIN_TYPE_UNIT);
        }
        if val.is_float() {
            return Some(format::BUILTIN_TYPE_FLOAT);
        }
        if !val.is_ptr() {
            return None;
        }
        self.heap.get(val.as_ptr_idx()).map(HeapObject::type_id)
    }

    pub(super) fn value_matches_type(&self, val: Value, type_id: u16) -> bool {
        self.runtime_type_id_of_value(val) == Some(type_id)
    }

    pub(super) fn dispatch_tycl(
        &mut self,
        op: Opcode,
        method_idx: usize,
        pc: &mut usize,
    ) -> VmResult {
        match op {
            Opcode::TyclDict => {
                let class_id = self.read_u16(method_idx, pc);
                let type_id_val = self.pop_stack()?;
                let type_id = if type_id_val.is_int() {
                    u16::try_from(type_id_val.as_int()).unwrap_or(u16::MAX)
                } else {
                    u16::MAX
                };
                let class_idx = usize::from(class_id);
                let class = self
                    .program
                    .module()
                    .classes
                    .get(class_idx)
                    .ok_or(VmError::NoInstance { class_id, type_id })?;
                let inst_idx = class
                    .instances
                    .iter()
                    .position(|inst| inst.type_id == type_id)
                    .ok_or(VmError::NoInstance { class_id, type_id })?;
                let dict = Value::from_int(
                    (i64::from(class_id) << 16) | i64::from(u16::try_from(inst_idx).unwrap_or(0)),
                );
                self.push_stack(dict)?;
            }
            Opcode::TyclCall => {
                let method_idx_op = usize::from(self.read_u8(method_idx, pc));
                let dict = self.pop_stack()?;
                if !dict.is_int() {
                    return Err(VmError::InvalidDictionary);
                }
                let packed = dict.as_int();
                let cls_idx =
                    usize::try_from(packed >> 16).map_err(|_| VmError::InvalidDictionary)?;
                let inst_idx =
                    usize::try_from(packed & 0xFFFF).map_err(|_| VmError::InvalidDictionary)?;
                let class = self
                    .program
                    .module()
                    .classes
                    .get(cls_idx)
                    .ok_or(VmError::InvalidDictionary)?;
                let instance = class
                    .instances
                    .get(inst_idx)
                    .ok_or(VmError::InvalidDictionary)?;
                let cm = instance
                    .methods
                    .get(method_idx_op)
                    .ok_or(VmError::InvalidDictionary)?;
                let callee = Value::from_int(i64::from(cm.method_idx));
                self.push_stack(callee)?;
            }
            _ => return Err(VmError::UnsupportedOpcode(op)),
        }
        Ok(())
    }
}
