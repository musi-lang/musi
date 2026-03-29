use std::collections::HashMap;

use music_il::SeamArtifact;

mod classes;
mod code;
mod constants;
mod effects;
mod foreign;
mod globals;
mod header;
mod methods;
mod strings;
mod types;
mod wire;

type SectionBytes = Vec<u8>;
type DecodedStrings = Vec<String>;
type StringOffsets = HashMap<u32, u32>;
type TextOffsets = HashMap<String, u32>;
type ConstantOffsets = HashMap<u16, u16>;

pub use wire::{
    ANON_METHOD_NAME, CLASS_SECTION_TAG, CONST_SECTION_TAG, EFFECT_SECTION_TAG, ENTRY_METHOD_NAME,
    FOREIGN_SECTION_TAG, GLOBAL_SECTION_TAG, HEADER_SIZE, MAGIC, METHOD_SECTION_TAG,
    STRING_SECTION_TAG, TYPE_SECTION_TAG, VERSION_MAJOR, VERSION_MINOR,
};

use crate::{CodecError, CodecResult};

/// Decode a `.seam` artifact from its binary representation.
///
/// # Errors
/// Returns an error when the header, section layout, opcode stream, or
/// referenced metadata is invalid or truncated.
pub fn decode_binary(data: &[u8]) -> CodecResult<SeamArtifact> {
    let section_count = header::decode_header(data)?;

    let mut strings = DecodedStrings::new();
    let mut offsets = StringOffsets::new();
    let mut constants = music_il::ConstantPool::new();
    let (mut methods, mut globals, mut types, mut effects, mut classes, mut foreigns) =
        (vec![], vec![], vec![], vec![], vec![], vec![]);

    let mut position = HEADER_SIZE;
    for _ in 0..section_count {
        let tag = read_array::<4>(data, position).ok_or(CodecError::TruncatedSection)?;
        position += 4;
        let length = usize::try_from(read_u32(data, position).ok_or(CodecError::TruncatedSection)?)
            .map_err(|_| CodecError::TruncatedSection)?;
        position += 4;

        let section = data
            .get(position..position + length)
            .ok_or(CodecError::TruncatedSection)?;

        match tag {
            STRING_SECTION_TAG => {
                let decoded = strings::decode_string_pool(section);
                strings = decoded.0;
                offsets = decoded.1;
            }
            TYPE_SECTION_TAG => {
                types = types::decode_types(section, &strings, &offsets)?;
            }
            CONST_SECTION_TAG => {
                constants = constants::decode_constants(section, &strings, &offsets)?;
            }
            METHOD_SECTION_TAG => {
                methods = methods::decode_methods(section, &strings, &offsets)?;
            }
            GLOBAL_SECTION_TAG => {
                globals = globals::decode_globals(section, &strings, &offsets)?;
            }
            EFFECT_SECTION_TAG => {
                effects = effects::decode_effects(section, &strings, &offsets)?;
            }
            CLASS_SECTION_TAG => {
                classes = classes::decode_classes(section, &strings, &offsets)?;
            }
            FOREIGN_SECTION_TAG => {
                foreigns = foreign::decode_foreigns(section, &strings, &offsets)?;
            }
            _ => {}
        }

        position += length;
    }

    Ok(SeamArtifact {
        constants,
        methods,
        globals,
        types,
        effects,
        classes,
        foreigns,
    })
}

/// Encode a SEAM artifact into the `.seam` binary format.
///
/// # Errors
/// Returns an error when the artifact cannot fit in the binary section/index
/// limits or references a missing interned string.
pub fn encode_binary(artifact: &SeamArtifact) -> CodecResult<SectionBytes> {
    let mut output = vec![0; HEADER_SIZE];
    let mut section_count = 0_u32;
    let strings = strings::StringIndex::build(artifact)?;

    let sections = [
        (STRING_SECTION_TAG, strings.bytes.clone()),
        (
            TYPE_SECTION_TAG,
            types::encode_types(&artifact.types, &strings)?,
        ),
        (
            CONST_SECTION_TAG,
            constants::encode_constants(&artifact.constants, &strings)?,
        ),
        (
            METHOD_SECTION_TAG,
            methods::encode_methods(&artifact.methods, &strings)?,
        ),
        (
            GLOBAL_SECTION_TAG,
            globals::encode_globals(&artifact.globals, &strings)?,
        ),
        (
            EFFECT_SECTION_TAG,
            effects::encode_effects(&artifact.effects, &strings)?,
        ),
        (
            CLASS_SECTION_TAG,
            classes::encode_classes(&artifact.classes, &strings)?,
        ),
        (
            FOREIGN_SECTION_TAG,
            foreign::encode_foreigns(&artifact.foreigns, &strings)?,
        ),
    ];

    for (tag, bytes) in sections {
        if bytes.is_empty() {
            continue;
        }

        section_count = section_count
            .checked_add(1)
            .ok_or(CodecError::ModuleTooLarge)?;
        write_section(&mut output, tag, &bytes)?;
    }

    header::finalize_header(&mut output, section_count)?;
    Ok(output)
}

fn write_section(output: &mut SectionBytes, tag: [u8; 4], bytes: &[u8]) -> CodecResult<()> {
    output.extend_from_slice(&tag);
    let length = u32::try_from(bytes.len()).map_err(|_| CodecError::ModuleTooLarge)?;
    output.extend_from_slice(&length.to_le_bytes());
    output.extend_from_slice(bytes);
    Ok(())
}

fn read_u16(data: &[u8], position: usize) -> Option<u16> {
    Some(u16::from_le_bytes(read_array::<2>(data, position)?))
}

fn read_u32(data: &[u8], position: usize) -> Option<u32> {
    Some(u32::from_le_bytes(read_array::<4>(data, position)?))
}

fn read_array<const N: usize>(data: &[u8], position: usize) -> Option<[u8; N]> {
    data.get(position..position + N)?.try_into().ok()
}

#[cfg(test)]
mod tests;
