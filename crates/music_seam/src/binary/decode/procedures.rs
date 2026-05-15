use super::*;

pub(super) fn decode_procedures(
    cursor: &mut Cursor<'_>,
    artifact: &mut Artifact,
) -> AssemblyResult {
    require_section(cursor, SectionTag::Procedures)?;
    for _ in 0..cursor.read_u32()? {
        let descriptor = decode_procedure(cursor)?;
        let _ = artifact.procedures.alloc(descriptor);
    }
    Ok(())
}

fn decode_procedure(cursor: &mut Cursor<'_>) -> AssemblyResult<ProcedureDescriptor> {
    let name = cursor.read_idx()?;
    let params = cursor.read_u16()?;
    let locals = cursor.read_u16()?;
    let param_tys = read_idx_list(cursor)?;
    let local_tys = read_idx_list(cursor)?;
    let result_tys = read_idx_list(cursor)?;
    let entry_label = cursor.read_u16()?;
    let bytecode_body = cursor.read_u32()?;
    let block_signature_table =
        decode_optional_table_id(cursor, "invalid procedure block table marker")?;
    let root_map_table = decode_optional_table_id(cursor, "invalid procedure root map marker")?;
    let domain_requirements = read_idx_list(cursor)?;
    let calling_convention = decode_calling_convention(cursor)?;
    let visibility = decode_visibility(cursor)?;
    let export = cursor.read_u8()? != 0;
    let hot = cursor.read_u8()? != 0;
    let cold = cursor.read_u8()? != 0;
    let labels = read_idx_list(cursor)?;
    let code = decode_procedure_code_entries(cursor)?;

    let mut descriptor = ProcedureDescriptor::new(name, params, locals, code.into_boxed_slice())
        .with_param_tys(param_tys.into_boxed_slice())
        .with_local_tys(local_tys.into_boxed_slice())
        .with_result_tys(result_tys.into_boxed_slice())
        .with_entry_label(entry_label)
        .with_bytecode_body(bytecode_body)
        .with_domain_requirements(domain_requirements.into_boxed_slice())
        .with_calling_convention(calling_convention)
        .with_visibility(visibility)
        .with_export(export)
        .with_hot(hot)
        .with_cold(cold)
        .with_labels(labels.into_boxed_slice());
    if let Some(block_signature_table) = block_signature_table {
        descriptor = descriptor.with_block_signature_table(Idx::from_raw(block_signature_table));
    }
    if let Some(root_map_table) = root_map_table {
        descriptor = descriptor.with_root_map_table(Idx::from_raw(root_map_table));
    }
    Ok(descriptor)
}

fn read_idx_list<T>(cursor: &mut Cursor<'_>) -> AssemblyResult<Vec<Idx<T>>> {
    let count = usize::from(cursor.read_u16()?);
    let mut ids = Vec::with_capacity(count);
    for _ in 0..count {
        ids.push(cursor.read_idx()?);
    }
    Ok(ids)
}

fn decode_optional_table_id(
    cursor: &mut Cursor<'_>,
    invalid_marker_message: &'static str,
) -> AssemblyResult<Option<u32>> {
    match cursor.read_u8()? {
        0 => Ok(None),
        1 => Ok(Some(cursor.read_u32()?)),
        _ => Err(AssemblyError::text_parse_source(invalid_marker_message)),
    }
}

fn decode_calling_convention(
    cursor: &mut Cursor<'_>,
) -> AssemblyResult<ProcedureCallingConvention> {
    ProcedureCallingConvention::from_wire(cursor.read_u8()?)
        .ok_or_else(|| AssemblyError::text_parse_source("unknown procedure calling convention"))
}

fn decode_visibility(cursor: &mut Cursor<'_>) -> AssemblyResult<ProcedureVisibility> {
    ProcedureVisibility::from_wire(cursor.read_u8()?)
        .ok_or_else(|| AssemblyError::text_parse_source("unknown procedure visibility"))
}

fn decode_procedure_code_entries(cursor: &mut Cursor<'_>) -> AssemblyResult<Vec<CodeEntry>> {
    let code_count = read_len(cursor, "code entry count")?;
    let mut code = Vec::with_capacity(code_count);
    for _ in 0..code_count {
        code.push(decode_procedure_code_entry(cursor)?);
    }
    Ok(code)
}

fn decode_procedure_code_entry(cursor: &mut Cursor<'_>) -> AssemblyResult<CodeEntry> {
    match cursor.read_u8()? {
        0 => Ok(CodeEntry::Label(Label {
            id: cursor.read_u16()?,
        })),
        1 => {
            let opcode_code = decode_opcode(cursor)?;
            let Some(opcode) = Opcode::from_wire_code(opcode_code) else {
                return Err(AssemblyError::UnknownOpcode(opcode_code));
            };
            let operand = decode_operand(cursor)?;
            Ok(CodeEntry::Instruction(Instruction::new(opcode, operand)))
        }
        _ => Err(AssemblyError::text_parse_source("unknown code entry kind")),
    }
}
