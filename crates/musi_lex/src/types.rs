use musi_core::DiagnosticBag;

use crate::token::Token;

/// Stream of tokens and resulting diagnostics.
pub type TokenStream = (Vec<Token>, DiagnosticBag);
