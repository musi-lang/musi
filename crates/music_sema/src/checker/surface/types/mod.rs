mod canonical;
mod import;
mod lower;
mod simple;

pub use canonical::{canonical_surface_ty, surface_key};
pub use import::import_surface_ty;
pub(in crate::checker::surface) use lower::{SurfaceTyBuilder, lower_surface_effect_row};
