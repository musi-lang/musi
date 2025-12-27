use std::sync::Arc;

use crate::{diagnostic::Diagnostic, source::SourceFile};

pub type Diagnostics = Vec<Diagnostic>;

pub type Ident = u32;
pub type Idents = Vec<Ident>;
pub type OptIdent = Option<Ident>;

pub type ArcSource = Arc<SourceFile>;
pub type OptArcSource<'a> = Option<&'a ArcSource>;
