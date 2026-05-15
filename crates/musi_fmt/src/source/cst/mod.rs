use std::ops::Range;

use music_syntax::SyntaxTree;

use crate::FormatOptions;

mod helpers;
mod layout;
mod spacing;
mod state;
mod token;
mod traversal;

use helpers::{is_let_line, next_non_comma_token_kind};
use state::{
    BraceFrame, BraceKind, CstFormatter, CstLeafRole, DeclarationState, ParenFrame, ParenKind,
    PendingAttachment, TokenWriteOptions,
};

pub(super) fn format_cst_source(
    source: &str,
    tree: &SyntaxTree,
    options: &FormatOptions,
    protected_ranges: Vec<Range<usize>>,
) -> String {
    traversal::format_cst_source(source, tree, options, protected_ranges)
}
