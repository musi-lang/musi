use std::collections::HashMap;

mod decode;
mod encode;
mod wire;

type SectionBytes = Vec<u8>;
type StringPool = Vec<String>;
type StringOffsets = HashMap<u32, u32>;
type TextOffsets = HashMap<String, u32>;
type ConstantOffsets = HashMap<u16, u16>;

pub use decode::decode_binary;
pub use encode::encode_binary;
pub use wire::{
    ANON_METHOD_NAME, CLASS_SECTION_TAG, CONST_SECTION_TAG, EFFECT_SECTION_TAG, ENTRY_METHOD_NAME,
    FOREIGN_SECTION_TAG, GLOBAL_SECTION_TAG, HEADER_SIZE, MAGIC, METHOD_SECTION_TAG,
    STRING_SECTION_TAG, TYPE_SECTION_TAG, VERSION_MAJOR, VERSION_MINOR,
};

#[cfg(test)]
mod tests;
