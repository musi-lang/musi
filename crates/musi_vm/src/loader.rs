#![allow(clippy::arithmetic_side_effects)]

use music_il::format::{self, HEADER_SIZE};
use music_il::opcode::Opcode;

use crate::error::LoadError;
use crate::module::{GlobalDef, Method, Module};
use crate::value::Value;

/// Decode a `.seam` binary slice into a [`Module`].
///
/// # Errors
/// Returns a [`LoadError`] if the binary is malformed, has an unsupported
/// version, contains an invalid opcode, or is truncated.
pub fn load(data: &[u8]) -> Result<Module, LoadError> {
    if data.len() < HEADER_SIZE {
        return Err(LoadError::TruncatedHeader);
    }

    let magic = read_bytes::<4>(data, 0).ok_or(LoadError::TruncatedHeader)?;
    if magic != format::MAGIC {
        return Err(LoadError::InvalidMagic);
    }

    let major = *data.get(4).ok_or(LoadError::TruncatedHeader)?;
    let minor = *data.get(5).ok_or(LoadError::TruncatedHeader)?;
    if major != format::VERSION_MAJOR || minor != format::VERSION_MINOR {
        return Err(LoadError::UnsupportedVersion { major, minor });
    }

    let section_count = read_u32_le(data, 8).ok_or(LoadError::TruncatedHeader)?;

    let mut strings: Vec<String> = Vec::new();
    let mut constants: Vec<Value> = Vec::new();
    let mut methods: Vec<Method> = Vec::new();
    let mut globals: Vec<GlobalDef> = Vec::new();

    let mut pos = HEADER_SIZE;
    for _ in 0..section_count {
        let tag = read_bytes::<4>(data, pos).ok_or(LoadError::TruncatedSection)?;
        pos = pos.wrapping_add(4);

        let length = usize::try_from(read_u32_le(data, pos).ok_or(LoadError::TruncatedSection)?)
            .map_err(|_| LoadError::TruncatedSection)?;
        pos = pos.wrapping_add(4);

        let section_data = data
            .get(pos..pos.wrapping_add(length))
            .ok_or(LoadError::TruncatedSection)?;

        match tag {
            format::section::STRT => {
                strings = decode_strt(section_data);
            }
            format::section::CNST => {
                constants = decode_cnst(section_data)?;
            }
            format::section::METH => {
                methods = decode_meth(section_data)?;
            }
            format::section::GLOB => {
                globals = decode_glob(section_data)?;
            }
            _ => {
                // Unknown section: skip for forward-compatibility
            }
        }

        pos = pos.wrapping_add(length);
    }

    Ok(Module {
        constants,
        strings,
        methods,
        globals,
    })
}

fn decode_strt(data: &[u8]) -> Vec<String> {
    data.split(|&b| b == 0)
        .filter(|s| !s.is_empty())
        .map(|s| String::from_utf8_lossy(s).into_owned())
        .collect()
}

fn decode_cnst(data: &[u8]) -> Result<Vec<Value>, LoadError> {
    let count_bytes = read_bytes::<2>(data, 0).ok_or(LoadError::TruncatedSection)?;
    let count = usize::from(u16::from_le_bytes(count_bytes));
    let mut pos = 2usize;
    let mut out = Vec::with_capacity(count);

    for _ in 0..count {
        let tag = *data.get(pos).ok_or(LoadError::TruncatedSection)?;
        pos = pos.wrapping_add(1);

        match tag {
            0x01 => {
                // Int: 8-byte LE i64
                let bytes = read_bytes::<8>(data, pos).ok_or(LoadError::TruncatedSection)?;
                out.push(Value::from_int(i64::from_le_bytes(bytes)));
                pos = pos.wrapping_add(8);
            }
            0x02 => {
                // Float: 8-byte LE u64 bits
                let bytes = read_bytes::<8>(data, pos).ok_or(LoadError::TruncatedSection)?;
                out.push(Value::from_float(f64::from_bits(u64::from_le_bytes(bytes))));
                pos = pos.wrapping_add(8);
            }
            0x03 => {
                // Str: 2-byte LE string-table offset; not usable as Value in Phase 1
                pos = pos.wrapping_add(2);
                out.push(Value::UNIT);
            }
            other => return Err(LoadError::InvalidConstantTag { tag: other }),
        }
    }

    Ok(out)
}

fn decode_meth(data: &[u8]) -> Result<Vec<Method>, LoadError> {
    let count_bytes = read_bytes::<2>(data, 0).ok_or(LoadError::TruncatedSection)?;
    let count = usize::from(u16::from_le_bytes(count_bytes));
    let mut pos = 2usize;
    let mut out = Vec::with_capacity(count);

    for _ in 0..count {
        let name_bytes = read_bytes::<4>(data, pos).ok_or(LoadError::TruncatedSection)?;
        let name = u32::from_le_bytes(name_bytes);
        pos = pos.wrapping_add(4);

        let locals_bytes = read_bytes::<2>(data, pos).ok_or(LoadError::TruncatedSection)?;
        let locals_count = u16::from_le_bytes(locals_bytes);
        pos = pos.wrapping_add(2);

        let instr_bytes = read_bytes::<2>(data, pos).ok_or(LoadError::TruncatedSection)?;
        let instr_count = u16::from_le_bytes(instr_bytes);
        pos = pos.wrapping_add(2);

        let code = scan_method_bytes(data, &mut pos, instr_count)?;
        out.push(Method {
            name,
            locals_count,
            code,
        });
    }

    Ok(out)
}

fn decode_glob(data: &[u8]) -> Result<Vec<GlobalDef>, LoadError> {
    let count_bytes = read_bytes::<2>(data, 0).ok_or(LoadError::TruncatedSection)?;
    let count = usize::from(u16::from_le_bytes(count_bytes));
    let mut pos = 2usize;
    let mut out = Vec::with_capacity(count);

    for _ in 0..count {
        let name_bytes = read_bytes::<4>(data, pos).ok_or(LoadError::TruncatedSection)?;
        let name = u32::from_le_bytes(name_bytes);
        pos = pos.wrapping_add(4);

        let flags = *data.get(pos).ok_or(LoadError::TruncatedSection)?;
        pos = pos.wrapping_add(1);

        out.push(GlobalDef {
            name,
            exported: (flags & 0x01) != 0,
            opaque: (flags & 0x02) != 0,
        });
    }

    Ok(out)
}

/// Scan `instr_count` encoded instructions starting at `pos`, advancing `pos`
/// and returning the raw bytes. The wire format stores instruction count, not
/// byte count, so we must decode each opcode to know how many bytes to skip.
fn scan_method_bytes(data: &[u8], pos: &mut usize, instr_count: u16) -> Result<Vec<u8>, LoadError> {
    let start = *pos;
    for _ in 0..instr_count {
        let byte_offset = *pos;
        let opcode_byte = *data.get(*pos).ok_or(LoadError::TruncatedSection)?;
        *pos = pos.wrapping_add(1);

        let op = Opcode::from_byte(opcode_byte).ok_or(LoadError::InvalidOpcode {
            byte: opcode_byte,
            offset: byte_offset,
        })?;

        let extra = operand_extra_bytes(op, data, *pos)?;
        if data.get(*pos..*pos + extra).is_none() {
            return Err(LoadError::TruncatedSection);
        }
        *pos = pos.wrapping_add(extra);
    }
    data.get(start..*pos)
        .map(<[u8]>::to_vec)
        .ok_or(LoadError::TruncatedSection)
}

/// Returns the number of operand bytes following the opcode byte.
fn operand_extra_bytes(op: Opcode, data: &[u8], pos: usize) -> Result<usize, LoadError> {
    match op {
        // U8 operand (1 byte)
        Opcode::LdLoc
        | Opcode::StLoc
        | Opcode::Call
        | Opcode::CallTail
        | Opcode::ClsUpv
        | Opcode::ArrTag
        | Opcode::TyTag
        | Opcode::EffResume
        | Opcode::ClsCall => Ok(1),

        // U16 or I16 operand (2 bytes)
        Opcode::LdCst
        | Opcode::LdGlb
        | Opcode::LdUpv
        | Opcode::StGlb
        | Opcode::StUpv
        | Opcode::LdLocW
        | Opcode::StLocW
        | Opcode::LdSmi
        | Opcode::BrTrue
        | Opcode::BrFalse
        | Opcode::BrJmp
        | Opcode::BrBack
        | Opcode::ArrNew
        | Opcode::ArrGeti
        | Opcode::ArrSeti
        | Opcode::TyChk
        | Opcode::TyCast
        | Opcode::EffPush
        | Opcode::EffNeed
        | Opcode::ClsDict
        | Opcode::FfiCall => Ok(2),

        // Wide (u16 + u8) and Tagged (u8 + u16) operands are both 3 bytes
        Opcode::ClsNew | Opcode::ArrNewt => Ok(3),

        // Variable: u16 count + count * i16
        Opcode::BrTbl => {
            let count_bytes = read_bytes::<2>(data, pos).ok_or(LoadError::TruncatedSection)?;
            let count = usize::from(u16::from_le_bytes(count_bytes));
            let offsets = count.checked_mul(2).ok_or(LoadError::TruncatedSection)?;
            offsets.checked_add(2).ok_or(LoadError::TruncatedSection)
        }

        // No operand
        _ => Ok(0),
    }
}

/// Read exactly `N` bytes from `data` at `pos`, returning them as a fixed-size array.
fn read_bytes<const N: usize>(data: &[u8], pos: usize) -> Option<[u8; N]> {
    data.get(pos..pos.wrapping_add(N))?.try_into().ok()
}

fn read_u32_le(data: &[u8], pos: usize) -> Option<u32> {
    Some(u32::from_le_bytes(read_bytes::<4>(data, pos)?))
}

#[cfg(test)]
mod tests;
