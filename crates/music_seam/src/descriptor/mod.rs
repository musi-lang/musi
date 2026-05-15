mod block_signature;
mod closure;
mod constant;
mod data;
mod export;
mod foreign;
mod global;
mod import;
mod manifest;
mod meta;
mod procedure;
mod root_map;
mod shape;
mod stack_effect;
mod type_desc;

pub use block_signature::BlockSignatureDescriptor;
pub use closure::ClosureDescriptor;
pub use constant::{ConstantDescriptor, ConstantValue};
pub use data::{
    DataDescriptor, DataFieldDescriptor, DataVariantDescriptor, ObjectHeaderDescriptor,
};
pub use export::{ExportDescriptor, ExportTarget};
pub use foreign::ForeignDescriptor;
pub use global::GlobalDescriptor;
pub use import::ImportDescriptor;
pub use manifest::ManifestDescriptor;
pub use meta::MetaDescriptor;
pub use procedure::{
    ProcedureCallingConvention, ProcedureDescriptor, ProcedureDomainList, ProcedureTypeIdList,
    ProcedureVisibility,
};
pub use root_map::{RootMapDescriptor, SafePointKind};
pub use shape::ShapeDescriptor;
pub use stack_effect::StackEffectDescriptor;
pub use type_desc::TypeDescriptor;
