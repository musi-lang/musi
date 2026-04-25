use music_hir::HirTyId;
use music_names::Symbol;

use super::{Attr, ConstraintSurface, SurfaceTyId};

pub type NameList = Box<[Box<str>]>;
pub type SymbolList = Box<[Symbol]>;
pub type AttrList = Box<[Attr]>;
pub type ConstraintSurfaceList = Box<[ConstraintSurface]>;
pub type HirTyIdList = Box<[HirTyId]>;
pub type SurfaceTyIdList = Box<[SurfaceTyId]>;
pub type ComptimeParamList = Box<[bool]>;
