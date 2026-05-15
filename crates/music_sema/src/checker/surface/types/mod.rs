mod canonical;
mod import;
mod lower;
mod simple;

pub use canonical::surface_key;
pub use import::import_surface_ty;
pub(in crate::checker::surface) use lower::SurfaceTyBuilder;
