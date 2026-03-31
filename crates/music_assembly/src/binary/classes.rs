use music_il::{ClassDescriptor, ClassDescriptors, ClassInstance, ClassInstances, ClassMethod};

use crate::binary::strings::StringIndex;

use super::*;

pub(super) fn decode_classes(
    data: &[u8],
    strings: &DecodedStrings,
    offsets: &StringOffsets,
) -> CodecResult<ClassDescriptors> {
    let count = usize::from(read_u16(data, 0).ok_or(CodecError::TruncatedSection)?);
    let mut position = 2_usize;
    let mut classes = Vec::with_capacity(count);

    for _ in 0..count {
        let id = read_u16(data, position).ok_or(CodecError::TruncatedSection)?;
        position += 2;
        let name = strings::read_string_ref(data, &mut position, strings, offsets)?;
        let method_count =
            usize::from(read_u16(data, position).ok_or(CodecError::TruncatedSection)?);
        position += 2;
        let mut method_names = Vec::with_capacity(method_count);
        for _ in 0..method_count {
            method_names.push(strings::read_string_ref(
                data,
                &mut position,
                strings,
                offsets,
            )?);
        }
        let instance_count =
            usize::from(read_u16(data, position).ok_or(CodecError::TruncatedSection)?);
        position += 2;
        let mut instances = ClassInstances::with_capacity(instance_count);
        for _ in 0..instance_count {
            let type_id = read_u16(data, position).ok_or(CodecError::TruncatedSection)?;
            position += 2;
            let impl_count =
                usize::from(read_u16(data, position).ok_or(CodecError::TruncatedSection)?);
            position += 2;
            let mut methods = Vec::with_capacity(impl_count);
            for _ in 0..impl_count {
                let name = strings::read_string_ref(data, &mut position, strings, offsets)?;
                let method_idx = read_u16(data, position).ok_or(CodecError::TruncatedSection)?;
                position += 2;
                methods.push(ClassMethod { name, method_idx });
            }
            instances.push(ClassInstance {
                ty_id: type_id,
                methods,
            });
        }
        classes.push(ClassDescriptor {
            id,
            name,
            method_names,
            instances,
        });
    }

    Ok(classes)
}

pub(super) fn encode_classes(
    classes: &ClassDescriptors,
    strings: &StringIndex,
) -> CodecResult<SectionBytes> {
    if classes.is_empty() {
        return Ok(Vec::new());
    }

    let mut output = Vec::new();
    let count = u16::try_from(classes.len()).map_err(|_| CodecError::ModuleTooLarge)?;
    output.extend_from_slice(&count.to_le_bytes());

    for class in classes {
        output.extend_from_slice(&class.id.to_le_bytes());
        output.extend_from_slice(&strings.offset(&class.name)?.to_le_bytes());
        let method_count =
            u16::try_from(class.method_names.len()).map_err(|_| CodecError::ModuleTooLarge)?;
        output.extend_from_slice(&method_count.to_le_bytes());
        for method_name in &class.method_names {
            output.extend_from_slice(&strings.offset(method_name)?.to_le_bytes());
        }
        let instance_count =
            u16::try_from(class.instances.len()).map_err(|_| CodecError::ModuleTooLarge)?;
        output.extend_from_slice(&instance_count.to_le_bytes());
        for instance in &class.instances {
            output.extend_from_slice(&instance.ty_id.to_le_bytes());
            let impl_count =
                u16::try_from(instance.methods.len()).map_err(|_| CodecError::ModuleTooLarge)?;
            output.extend_from_slice(&impl_count.to_le_bytes());
            for method in &instance.methods {
                output.extend_from_slice(&strings.offset(&method.name)?.to_le_bytes());
                output.extend_from_slice(&method.method_idx.to_le_bytes());
            }
        }
    }

    Ok(output)
}
