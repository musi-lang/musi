use std::collections::{BTreeMap, BTreeSet};
use std::error::Error;
use std::fmt::{self, Display, Formatter, Result as FmtResult, Write as _};
use std::iter::{Enumerate, Peekable};
use std::str::Lines;

use music_base::diag::{CatalogDiagnostic, DiagContext};

use crate::{AssemblyError, SeamDiagKind, diag::hil_verify_error_kind};

type HilName = Box<str>;
type HilTypeMap = BTreeMap<HilValueId, HilType>;
type HilBlockSet = BTreeSet<HilName>;
type HilLineCursor<'a> = Peekable<Enumerate<Lines<'a>>>;
type HilLineCursorRef<'cursor, 'text> = &'cursor mut HilLineCursor<'text>;

mod format;
mod model;
mod parse;
mod verify;

pub use format::format_hil;
pub use model::{
    HilBinaryOp, HilBlock, HilFunction, HilInstruction, HilModule, HilParam, HilShape,
    HilTerminator, HilType, HilValueId, HilVerifyError, HilVerifyResult,
};
pub use parse::parse_hil;

fn clone_hil_name(name: &str) -> HilName {
    name.into()
}

fn clone_hil_type(ty: &HilType) -> HilType {
    HilType::new(ty.0.clone())
}
