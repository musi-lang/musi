use std::sync::Arc;

use crate::{diagnostic::Diagnostic, source::SourceFile};

// ============================================================================
// DIAGNOSTIC
// ============================================================================

pub type Diagnostics = Vec<Diagnostic>;

// ============================================================================
// IDENTIFIERS
// ============================================================================

pub type Ident = u32;
pub type Idents = Vec<Ident>;
pub type OptIdent = Option<Ident>;

// ============================================================================
// SOURCE
// ============================================================================

pub type ArcSource = Arc<SourceFile>;
pub type OptArcSource<'a> = Option<&'a ArcSource>;
