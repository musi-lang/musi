use std::collections::BTreeSet;

use super::*;

impl Artifact {
    pub(super) fn validate_root_maps(&self) -> Result<(), ArtifactError> {
        for (_, descriptor) in self.root_maps.iter() {
            self.require_string(descriptor.safe_point)?;
            let procedure = if let Some(procedure_id) = descriptor.procedure {
                self.require_procedure(procedure_id)?;
                Some((procedure_id, self.procedures.get(procedure_id)))
            } else {
                None
            };
            Self::validate_root_map_slot_list_len(
                descriptor.local_slots.as_ref(),
                "root map local slots",
            )?;
            Self::validate_root_map_slot_list_len(
                descriptor.stack_slots.as_ref(),
                "root map stack slots",
            )?;
            Self::validate_root_map_slot_list_len(
                descriptor.capture_slots.as_ref(),
                "root map capture slots",
            )?;
            Self::validate_root_map_slot_list_len(
                descriptor.defer_slots.as_ref(),
                "root map defer slots",
            )?;
            Self::validate_root_map_slot_list_len(
                descriptor.pin_slots.as_ref(),
                "root map pin slots",
            )?;
            Self::validate_unique_root_map_slots(
                descriptor.local_slots.as_ref(),
                "root map local slots",
            )?;
            Self::validate_unique_root_map_slots(
                descriptor.stack_slots.as_ref(),
                "root map stack slots",
            )?;
            Self::validate_unique_root_map_slots(
                descriptor.capture_slots.as_ref(),
                "root map capture slots",
            )?;
            Self::validate_unique_root_map_slots(
                descriptor.defer_slots.as_ref(),
                "root map defer slots",
            )?;
            Self::validate_unique_root_map_slots(
                descriptor.pin_slots.as_ref(),
                "root map pin slots",
            )?;
            if let Some((procedure_id, procedure)) = procedure {
                self.validate_root_map_safe_point_for_procedure(procedure, descriptor)?;
                self.validate_root_map_slots_for_procedure(procedure_id, procedure, descriptor)?;
            }
        }
        Ok(())
    }

    fn validate_root_map_safe_point_for_procedure(
        &self,
        procedure: &ProcedureDescriptor,
        descriptor: &RootMapDescriptor,
    ) -> Result<(), ArtifactError> {
        if procedure.labels.is_empty() {
            return Ok(());
        }
        let procedure_name = self.string_text(procedure.name);
        let safe_point = self.string_text(descriptor.safe_point);
        let matches_label = procedure.labels.iter().copied().any(|label| {
            let label_name = self.string_text(label);
            safe_point == format!("{procedure_name}.{label_name}")
                || safe_point == format!("{procedure_name}:{label_name}")
        });
        let matches_instruction_safe_point = safe_point
            .strip_prefix(&format!("{procedure_name}:sp"))
            .is_some_and(|suffix| {
                !suffix.is_empty() && suffix.chars().all(|ch| ch.is_ascii_digit())
            });
        if !matches_label && !matches_instruction_safe_point {
            return Err(ArtifactError::InvalidReference {
                table: "root map safe point",
            });
        }
        Ok(())
    }

    fn validate_root_map_slots_for_procedure(
        &self,
        procedure_id: ProcedureId,
        procedure: &ProcedureDescriptor,
        descriptor: &RootMapDescriptor,
    ) -> Result<(), ArtifactError> {
        let local_slot_limit = usize::from(procedure.locals.max(procedure.params));
        Self::validate_root_map_slot_bounds(
            descriptor.local_slots.as_ref(),
            local_slot_limit,
            "root map local slots",
        )?;
        if let Some(stack_slot_limit) = self.root_map_stack_slot_limit(procedure_id, procedure) {
            Self::validate_root_map_slot_bounds(
                descriptor.stack_slots.as_ref(),
                stack_slot_limit,
                "root map stack slots",
            )?;
        }
        if let Some(capture_slot_limit) = self.root_map_capture_slot_limit(procedure_id) {
            Self::validate_root_map_slot_bounds(
                descriptor.capture_slots.as_ref(),
                capture_slot_limit,
                "root map capture slots",
            )?;
        }
        Ok(())
    }

    fn root_map_stack_slot_limit(
        &self,
        procedure_id: ProcedureId,
        procedure: &ProcedureDescriptor,
    ) -> Option<usize> {
        if procedure.param_tys.len() != usize::from(procedure.params)
            || procedure.local_tys.len() != usize::from(procedure.locals)
        {
            return None;
        }
        let mut known_stack = None::<ProcedureTypeStack>;
        let mut max_depth = 0usize;
        let mut has_known_stack = false;
        for entry in &procedure.code {
            match entry {
                CodeEntry::Label(Label { id }) => {
                    known_stack = self
                        .block_signature_for_label(procedure_id, *id)
                        .map(block_signature_stack);
                    if let Some(stack) = known_stack.as_ref() {
                        has_known_stack = true;
                        max_depth = max_depth.max(stack.len());
                    }
                }
                CodeEntry::Instruction(instruction) => {
                    let Some(stack) = known_stack.as_mut() else {
                        continue;
                    };
                    has_known_stack = true;
                    max_depth = max_depth.max(stack.len());
                    match instruction.opcode {
                        Opcode::Br => {
                            if self
                                .verify_branch_stack(procedure_id, instruction, stack)
                                .is_err()
                            {
                                return None;
                            }
                            known_stack = None;
                        }
                        Opcode::BrZ => {
                            if self
                                .verify_branch_false_stack(procedure_id, instruction, stack)
                                .is_err()
                            {
                                return None;
                            }
                            max_depth = max_depth.max(stack.len());
                        }
                        Opcode::BrTbl => {
                            if self
                                .verify_branch_table_stack(procedure_id, instruction, stack)
                                .is_err()
                            {
                                return None;
                            }
                            known_stack = None;
                        }
                        Opcode::Ret => {
                            if Self::verify_return_stack(procedure, stack).is_err() {
                                return None;
                            }
                            known_stack = None;
                        }
                        _ => {
                            if self.apply_stack_effect(procedure, instruction, stack) {
                                max_depth = max_depth.max(stack.len());
                            } else {
                                known_stack = None;
                            }
                        }
                    }
                }
            }
        }
        has_known_stack.then_some(max_depth)
    }

    fn root_map_capture_slot_limit(&self, procedure_id: ProcedureId) -> Option<usize> {
        self.closures
            .iter()
            .filter_map(|(_, descriptor)| {
                (descriptor.procedure == procedure_id).then_some(
                    descriptor
                        .capture_tys
                        .len()
                        .max(usize::from(descriptor.capture_count)),
                )
            })
            .max()
    }

    fn validate_root_map_slot_list_len(
        slots: &[u16],
        table: &'static str,
    ) -> Result<(), ArtifactError> {
        if u16::try_from(slots.len()).is_err() {
            return Err(ArtifactError::SectionLimitExceeded { table });
        }
        Ok(())
    }

    fn validate_unique_root_map_slots(
        slots: &[u16],
        table: &'static str,
    ) -> Result<(), ArtifactError> {
        let mut seen = BTreeSet::new();
        for slot in slots.iter().copied() {
            if !seen.insert(slot) {
                return Err(ArtifactError::InvalidReference { table });
            }
        }
        Ok(())
    }

    fn validate_root_map_slot_bounds(
        slots: &[u16],
        slot_limit: usize,
        table: &'static str,
    ) -> Result<(), ArtifactError> {
        for slot in slots.iter().copied() {
            if usize::from(slot) >= slot_limit {
                return Err(ArtifactError::InvalidReference { table });
            }
        }
        Ok(())
    }
}
