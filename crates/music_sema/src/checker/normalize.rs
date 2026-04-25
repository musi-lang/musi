use music_arena::SliceRange;
use music_hir::HirTyField;

type HirTyFieldRange = SliceRange<HirTyField>;

mod constraints;
mod constructors;
mod effects;
mod lower;
mod matches;
mod params;
mod render;
mod symbols;
