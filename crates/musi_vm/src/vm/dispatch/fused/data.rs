use super::super::int::{int_from_value, int_overflow_error};
use super::super::shape_error::fused_dispatch_error;
use super::super::*;

impl Vm {
    pub(super) fn exec_fused_data(&mut self, fused: RuntimeFusedOp) -> VmResult<StepOutcome> {
        match fused {
            RuntimeFusedOp::LocalLdFldBranchTable {
                local,
                branch_table,
            } => self.exec_local_data_tag_branch_table(local, branch_table),
            RuntimeFusedOp::LocalLdFldConstStore {
                source,
                field,
                dest,
                fallthrough,
            } => self.exec_local_data_get_const_store(source, field, dest, fallthrough),
            RuntimeFusedOp::LocalNewObj1Init {
                field_local,
                tag,
                ty,
                data_local,
                match_local,
                zero,
                fallthrough,
            } => self.exec_local_data_new1_init(NewObjInitPlan {
                field_local,
                tag,
                ty,
                data_local,
                match_local,
                zero,
                fallthrough,
            }),
            RuntimeFusedOp::LocalCopyAddSmi {
                source,
                dest,
                smi,
                fallthrough,
            } => self.exec_local_copy_add_smi(source, dest, smi, fallthrough),
            _ => Err(fused_dispatch_error("data")),
        }
    }

    fn exec_local_data_new1_init(&mut self, plan: NewObjInitPlan) -> VmResult<StepOutcome> {
        let NewObjInitPlan {
            field_local,
            tag,
            ty,
            data_local,
            match_local,
            zero,
            fallthrough,
        } = plan;
        let field = self.local_value(field_local)?;
        let data = self.alloc_data_owned(ty, i64::from(tag), vec![field].into())?;
        self.store_local(data_local, data.clone())?;
        self.store_local(match_local, Value::Int(i64::from(zero)))?;
        self.store_local(match_local, data)?;
        self.jump_to_ip(fallthrough)?;
        Ok(StepOutcome::Continue)
    }

    fn exec_local_copy_add_smi(
        &mut self,
        source: u16,
        dest: u16,
        smi: i16,
        fallthrough: usize,
    ) -> VmResult<StepOutcome> {
        let value = self.local_value(source)?;
        let int = int_from_value(&value)?;
        self.store_local(dest, value)?;
        let result = int
            .checked_add(i64::from(smi))
            .ok_or_else(int_overflow_error)?;
        self.current_frame_mut()?.stack.push(Value::Int(result));
        self.jump_to_ip(fallthrough)?;
        Ok(StepOutcome::Continue)
    }

    fn exec_local_data_tag_branch_table(
        &mut self,
        local: u16,
        branch_table: usize,
    ) -> VmResult<StepOutcome> {
        let tag = {
            let frame = self.current_frame()?;
            let index = usize::from(local);
            let Some(value) = frame.locals.get(index) else {
                return Err(VmError::new(VmErrorKind::IndexOutOfBounds {
                    space: VmIndexSpace::Local,
                    owner: None,
                    index: i64::from(local),
                    len: frame.locals.len(),
                }));
            };
            let data = Self::expect_data(value.clone())?;
            self.heap.data(data)?.tag
        };
        self.branch_table_jump_at(branch_table, tag)?;
        Ok(StepOutcome::Continue)
    }

    fn exec_local_data_get_const_store(
        &mut self,
        source: u16,
        field: i16,
        dest: u16,
        fallthrough: usize,
    ) -> VmResult<StepOutcome> {
        let field_value = {
            let frame = self.current_frame()?;
            let source_index = usize::from(source);
            let Some(value) = frame.locals.get(source_index) else {
                return Err(VmError::new(VmErrorKind::IndexOutOfBounds {
                    space: VmIndexSpace::Local,
                    owner: None,
                    index: i64::from(source),
                    len: frame.locals.len(),
                }));
            };
            let data = Self::expect_data(value.clone())?;
            let data_ref = self.heap.data(data)?;
            let slot = usize::try_from(field).unwrap_or(usize::MAX);
            data_ref.fields.get(slot).cloned().ok_or_else(|| {
                VmError::new(VmErrorKind::InvalidDataIndex {
                    index: i64::from(field),
                    len: data_ref.fields.len(),
                })
            })?
        };
        let frame = self.current_frame_mut()?;
        let dest_index = usize::from(dest);
        let len = frame.locals.len();
        let Some(local) = frame.locals.get_mut(dest_index) else {
            return Err(VmError::new(VmErrorKind::IndexOutOfBounds {
                space: VmIndexSpace::Local,
                owner: None,
                index: i64::from(dest),
                len,
            }));
        };
        *local = field_value;
        frame.set_ip(fallthrough);
        Ok(StepOutcome::Continue)
    }
}

#[derive(Clone, Copy)]
struct NewObjInitPlan {
    field_local: u16,
    tag: i16,
    ty: TypeId,
    data_local: u16,
    match_local: u16,
    zero: i16,
    fallthrough: usize,
}
